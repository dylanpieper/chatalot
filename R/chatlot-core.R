#' Check if the installed version of ellmer is newer than 0.2.0
#'
#' @return Logical indicating whether the installed version is greater than 0.2.0
#' @keywords internal
#' @noRd
is_new_ellmer <- function() {
  utils::packageVersion("ellmer") > "0.2.0"
}

#' Capture chat model response with proper handling
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type Type specification for structured data
#' @return List containing response (chat, text, and structured data)
#' @keywords internal
#' @noRd
capture <- function(original_chat,
                    prompt,
                    type,
                    echo,
                    ...) {
  response <- NULL
  structured_data <- NULL
  chat <- original_chat$clone()

  result <- withCallingHandlers(
    {
      if (!is.null(type)) {
        structured_data <- tryCatch(
          {
            chat$chat_structured(prompt, type = type, ...)
          },
          error = function(e) {
            cli::cli_alert_warning("Initial extraction failed, retrying...")
            tryCatch(
              {
                chat$chat_structured(prompt, type = type, ...)
              },
              error = function(e) {
                NULL
              }
            )
          }
        )

        if (is.null(structured_data)) {
          stop("Structured data extraction is NULL. Try again or remove the 'type' argument to check for other errors.")
        }
      } else {
        response <- chat$chat(prompt, echo = echo, ...)

        if (is.null(response)) {
          stop("Received NULL chat response")
        }
      }

      list(
        chat = chat,
        text = response,
        structured_data = structured_data
      )
    },
    interrupt = function(e) {
      signalCondition(e)
    }
  )

  return(result)
}

#' Process lot of prompts with progress tracking
#' @param chat_obj Chat model object
#' @param prompts List of prompts
#' @param type Type specification for structured data
#' @param file Path to save state file (.rds)
#' @param progress Whether to show progress bars
#' @param beep Play sound on completion
#' @return Process results object
#' @keywords internal
#' @noRd
process_sequential <- function(
    chat_obj,
    prompts,
    type,
    file,
    progress,
    beep,
    echo,
    ...) {
  if (file.exists(file)) {
    result <- readRDS(file)
    if (!identical(as.list(prompts), result@prompts)) {
      cli::cli_alert_warning("Prompts don't match saved state. Starting fresh.")
      unlink(file)
      result <- NULL
    }
  } else {
    result <- NULL
  }

  if (is.null(result)) {
    orig_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"
    result <- process(
      prompts = as.list(prompts),
      responses = vector("list", length(prompts)),
      completed = 0L,
      file = file,
      type = type,
      progress = progress,
      input_type = orig_type,
      chunk_size = NULL,
      workers = NULL,
      state = NULL
    )
    saveRDS(result, file)
  }

  total_prompts <- length(prompts)

  if (result@completed >= total_prompts) {
    if (progress) {
      cli::cli_alert_success("Complete")
    }
    return(create_results(result))
  }

  pb <- NULL
  if (progress) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Processing chats [{cli::pb_current}/{cli::pb_total}] ",
        "[{cli::pb_bar}] {cli::pb_eta}"
      ),
      total = total_prompts
    )
    cli::cli_progress_update(id = pb, set = result@completed)
  }

  tryCatch({
    for (i in (result@completed + 1L):total_prompts) {
      response <- capture(
        chat_obj, prompts[[i]], type,
        echo = echo,
        ...
      )

      result@responses[[i]] <- response
      result@completed <- i
      saveRDS(result, file)

      if (!is.null(pb)) {
        cli::cli_progress_update(id = pb, set = i)
      }
    }

    finish_process(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(result, file)

    if (inherits(e, "interrupt")) {
      handle_interrupt(result, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(result, file)

    if (beep) beepr::beep("coin")
    cli::cli_alert_warning(sprintf(
      "Interrupted at chat %d of %d",
      result@completed, total_prompts
    ))
  }, finally = {
    if (!exists("result")) {
      result <- readRDS(file)
    }
  })

  create_results(result)
}

#' Process prompts in parallel chunks with error handling and state management
#' @param chat_obj Chat model object for API calls
#' @param prompts Vector or list of prompts to process
#' @param type Optional type specification for structured data extraction
#' @param file Path to save intermediate state
#' @param workers Number of parallel workers
#' @param chunk_size Number of prompts to process in parallel at a time
#' @param beep Play sound on completion/error
#' @param max_chunk_attempts Maximum retries per failed chunk
#' @param progress Whether to show progress bars
#' @return Process results object
#' @keywords internal
#' @noRd
process_future <- function(
    chat_obj,
    prompts,
    type,
    file,
    workers,
    chunk_size,
    max_chunk_attempts,
    beep,
    progress,
    echo,
    ...) {
  if (is_new_ellmer()) {
    cli::cli_abort(c(
      "`chat_future()` currently does not work in ellmer {packageVersion('ellmer')}",
      "!" = "This issue is temporary and will be fixed in a future ellmer release",
      "v" = "`chat_future()` will work if you install {.code pak::pak('ellmer@0.2.0')}",
      "!" = "Be aware that ellmer 0.2.0 exposes API keys in chat objects"
    ))
  }

  validate_chunk_result <- function(chunk_result, chunk_idx) {
    if (inherits(chunk_result, "error") || inherits(chunk_result, "worker_error")) {
      return(list(valid = FALSE, message = conditionMessage(chunk_result)))
    }

    if (!is.list(chunk_result) || !("responses" %in% names(chunk_result))) {
      return(list(valid = FALSE, message = sprintf("Invalid chunk structure in chunk %d", chunk_idx)))
    }

    if (length(chunk_result$responses) == 0) {
      return(list(valid = FALSE, message = sprintf("Empty responses in chunk %d", chunk_idx)))
    }

    null_indices <- which(vapply(chunk_result$responses, is.null, logical(1)))
    if (length(null_indices) > 0) {
      return(list(
        valid = FALSE,
        message = sprintf(
          "NULL responses at indices %s in chunk %d",
          paste(null_indices, collapse = ", "), chunk_idx
        )
      ))
    }

    list(valid = TRUE, message = NULL)
  }

  total_prompts <- length(prompts)
  prompts_list <- as.list(prompts)
  original_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"

  if (file.exists(file)) {
    result <- readRDS(file)
    if (!identical(prompts_list, result@prompts)) {
      cli::cli_alert_warning("Prompts don't match saved state. Starting fresh.")
      unlink(file)
      result <- NULL
    }
  } else {
    result <- NULL
  }

  if (is.null(result)) {
    result <- process(
      prompts = prompts_list,
      responses = vector("list", total_prompts),
      completed = 0L,
      file = file,
      type = type,
      progress = progress,
      input_type = original_type,
      chunk_size = as.integer(chunk_size),
      workers = as.integer(workers),
      state = list(
        active_workers = 0L,
        failed_chunks = list(),
        retry_count = 0L
      )
    )
    saveRDS(result, file)
  }

  if (result@completed >= total_prompts) {
    if (progress) {
      cli::cli_alert_success("Complete")
    }
    return(create_results(result))
  }

  future::plan(future::multisession, workers = workers)

  remaining_prompts <- prompts[(result@completed + 1L):total_prompts]
  chunks <- split(remaining_prompts, ceiling(seq_along(remaining_prompts) / chunk_size))

  pb <- NULL
  if (progress) {
    pb <- cli::cli_progress_bar(
      format = "Processing chats [{cli::pb_current}/{cli::pb_total}] [{cli::pb_bar}] {cli::pb_eta}",
      total = total_prompts
    )
    cli::cli_progress_update(id = pb, set = result@completed)
  }

  capture_future <- capture

  tryCatch({
    for (chunk_idx in seq_along(chunks)) {
      chunk <- chunks[[chunk_idx]]
      retry_count <- 0
      success <- FALSE
      last_error <- NULL

      while (!success && retry_count < max_chunk_attempts) {
        retry_count <- retry_count + 1
        worker_chat <- chat_obj$clone()

        chunk_result <-
          withCallingHandlers(
            tryCatch(
              {
                responses <- NULL
                tryCatch(
                  {
                    responses <- furrr::future_map(
                      chunk,
                      function(prompt) {
                        capture_future(
                          worker_chat,
                          prompt,
                          type,
                          echo = echo,
                          ...
                        )
                      },
                      .options = furrr::furrr_options(
                        scheduling = 1,
                        seed = TRUE
                      )
                    )

                    list(
                      success = TRUE,
                      responses = responses
                    )
                  },
                  error = function(e) {
                    error_msg <- conditionMessage(e)
                    if (grepl("Caused by error", error_msg)) {
                      error_msg <- gsub(".*\\!\\s*", "", error_msg)
                    }

                    stop(error_msg, call. = FALSE)
                  }
                )
              },
              error = function(e) {
                last_error <- e
                stop(conditionMessage(e),
                  call. = FALSE, domain = "process_future"
                )
                e_class <- class(e)[1]
                cli::cli_alert_warning(sprintf(
                  "Error in chunk processing (%s): %s",
                  e_class, conditionMessage(e)
                ))
                structure(
                  list(
                    success = FALSE,
                    error = "other",
                    message = conditionMessage(e)
                  ),
                  class = c("worker_error", "error")
                )
              }
            )
          )

        validation <- validate_chunk_result(chunk_result, chunk_idx)
        success <- validation$valid

        if (success) {
          start_idx <- result@completed + 1
          end_idx <- result@completed + length(chunk)

          result@responses[start_idx:end_idx] <- chunk_result$responses

          result@completed <- end_idx
          saveRDS(result, file)
          if (!is.null(pb)) {
            cli::cli_progress_update(id = pb, set = result@completed)
          }
        } else {
          success <- FALSE
          break
        }
      }

      if (!success) {
        error_msg <- if (!is.null(last_error)) {
          sprintf(
            "Chunk %d failed after %d attempts. Last error: %s",
            chunk_idx, max_chunk_attempts, conditionMessage(last_error)
          )
        } else {
          sprintf(
            "Chunk %d failed after %d attempts: %s",
            chunk_idx, max_chunk_attempts, validation$message
          )
        }
        stop(error_msg)
      }
    }

    finish_process(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(result, file)

    if (inherits(e, "interrupt")) {
      handle_interrupt(result, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(result, file)

    if (beep) beepr::beep("coin")
    cli::cli_alert_warning(sprintf(
      "Interrupted at chat %d of %d",
      result@completed, total_prompts
    ))
  }, finally = {
    if (!exists("result")) {
      result <- readRDS(file)
    }
    future::plan(future::sequential)
  })

  create_results(result)
}

#' Process chunks of prompts in parallel
#' @param chunks List of prompt chunks to process
#' @param result A process object to store results
#' @param chat_obj Chat model object for making API calls
#' @param type Type specification for structured data extraction
#' @param pb Progress bar object
#' @param file Path to save intermediate state
#' @param progress Whether to show progress bars
#' @param beep Logical indicating whether to play sounds
#' @return Updated process object with processed results
#' @keywords internal
#' @noRd
process_chunks <- function(chunks,
                           result,
                           chat_obj,
                           type,
                           pb,
                           file,
                           progress,
                           beep,
                           echo,
                           ...) {
  was_interrupted <- FALSE
  capture_future <- capture

  for (chunk in chunks) {
    if (was_interrupted) break

    withCallingHandlers(
      {
        new_responses <- furrr::future_map(
          chunk,
          function(prompt) {
            worker_chat <- chat_obj$clone()
            capture_future(
              worker_chat,
              prompt,
              type,
              echo = echo,
              ...
            )
          },
          .progress = FALSE
        )

        start_idx <- result@completed + 1
        end_idx <- result@completed + length(new_responses)
        result@responses[start_idx:end_idx] <- new_responses
        result@completed <- end_idx
        saveRDS(result, file)
        if (!is.null(pb)) {
          cli::cli_progress_update(id = pb, set = end_idx)
        }
      },
      interrupt = function(e) {
        was_interrupted <<- TRUE
        handle_interrupt(result, beep)
        invokeRestart("abort")
      }
    )
  }

  if (!was_interrupted) {
    finish_process(pb, beep, progress)
  }
}


#' Handle interruption
#' @name handle_interrupt
#' @usage handle_interrupt(result, beep)
#' @param result A process object containing processing state
#' @param beep Logical indicating whether to play a sound
#' @return NULL (called for side effects)
#' @keywords internal
#' @noRd
handle_interrupt <- function(result, beep) {
  cli::cli_alert_warning(
    sprintf(
      "Interrupted at chat %d of %d",
      result@completed, length(result@prompts)
    )
  )
  if (beep) beepr::beep("coin")
}

#' Finish successful processing
#' @description Called after successful completion of processing to update progress
#'   indicators and play a success sound
#' @param pb Progress bar object
#' @param beep Logical; whether to play success sound
#' @param progress Whether to show progress bars
#' @return NULL (invisibly)
#' @keywords internal
#' @noRd
finish_process <- function(pb, beep, progress) {
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }
  if (progress) {
    cli::cli_alert_success("Complete")
  }
  if (beep) beepr::beep("ping")
  invisible()
}

#' Create results object from process
#' @param result Process object
#' @return Results object with class "process"
#' @keywords internal
#' @noRd
create_results <- function(result) {
  base_list <- list(
    prompts = result@prompts,
    responses = result@responses,
    completed = result@completed,
    file = result@file,
    type = result@type
  )

  base_list$texts <- function() texts(result)
  base_list$chats <- function() chats(result)
  base_list$progress <- function() progress(result)

  structure(base_list, class = "process")
}
