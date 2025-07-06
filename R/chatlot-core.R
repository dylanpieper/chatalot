#' Capture chat model response with proper handling
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type Type specification for structured data
#' @param eval If TRUE, performs one evaluation round for structured data extraction
#' @return List containing response information
#' @keywords internal
#' @noRd
capture <- function(original_chat,
                    prompt,
                    type,
                    eval,
                    echo,
                    ...) {
  response <- NULL
  structured_data <- NULL
  chat <- original_chat$clone()

  result <- withCallingHandlers(
    {
      if (!is.null(type)) {
        result <- process_evaluations(chat, prompt, type, eval, echo = echo, ...)
        structured_data <- result$final
        chat <- result$chat

        if (is.null(structured_data)) {
          stop("Received NULL structured data response")
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
#' @param eval If TRUE, performs one evaluation round for structured data extraction
#' @param file Path to save state file (.rds)
#' @param progress Whether to show progress bars
#' @param beep Play sound on completion
#' @return Lot results object
#' @keywords internal
#' @noRd
process_sequential <- function(
    chat_obj,
    prompts,
    type,
    eval,
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
    result <- lot(
      prompts = as.list(prompts),
      responses = vector("list", length(prompts)),
      completed = 0L,
      file = file,
      type = type,
      eval = eval,
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
        eval = eval,
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

    finish_successful_lot(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(result, file)

    if (inherits(e, "interrupt")) {
      handle_lot_interrupt(result, beep)
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
#' @param eval If TRUE, performs one evaluation round for structured data extraction
#' @param file Path to save intermediate state
#' @param workers Number of parallel workers
#' @param chunk_size Number of prompts to process in parallel at a time
#' @param beep Play sound on completion/error
#' @param max_chunk_attempts Maximum retries per failed chunk
#' @param progress Whether to show progress bars
#' @return Lot results object
#' @keywords internal
#' @noRd
process_future <- function(
    chat_obj,
    prompts,
    type,
    eval,
    file,
    workers,
    chunk_size,
    max_chunk_attempts,
    beep,
    progress,
    echo,
    ...) {
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
    result <- lot(
      prompts = prompts_list,
      responses = vector("list", total_prompts),
      completed = 0L,
      file = file,
      type = type,
      eval = eval,
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
                        chatlot:::capture(
                          worker_chat,
                          prompt,
                          type,
                          eval = eval,
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

    finish_successful_lot(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(result, file)

    if (inherits(e, "interrupt")) {
      handle_lot_interrupt(result, beep)
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
#' @param result A lot object to store results
#' @param chat_obj Chat model object for making API calls
#' @param type Type specification for structured data extraction
#' @param eval If TRUE, performs one evaluation round for structured data extraction
#' @param pb Progress bar object
#' @param file Path to save intermediate state
#' @param progress Whether to show progress bars
#' @param beep Logical indicating whether to play sounds
#' @return Updated lot object with processed results
#' @keywords internal
#' @noRd
process_chunks <- function(chunks,
                           result,
                           chat_obj,
                           type,
                           eval,
                           pb,
                           file,
                           progress,
                           beep,
                           echo,
                           ...) {
  was_interrupted <- FALSE

  for (chunk in chunks) {
    if (was_interrupted) break

    withCallingHandlers(
      {
        new_responses <- furrr::future_map(
          chunk,
          function(prompt) {
            worker_chat <- chat_obj$clone()
            chatlot:::capture(
              worker_chat,
              prompt,
              type,
              eval = eval,
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
        handle_lot_interrupt(result, beep)
        invokeRestart("abort")
      }
    )
  }

  if (!was_interrupted) {
    finish_successful_lot(pb, beep, progress)
  }
}

#' Process structured data extraction with evaluation
#' @param chat_obj Chat model object
#' @param prompt The prompt or text to analyze
#' @param type Type specification for structured data
#' @param eval If TRUE, performs one evaluation round for structured data extraction
#' @return List containing extraction process
#' @keywords internal
#' @noRd
process_evaluations <- function(chat_obj, prompt, type, eval = FALSE, echo = FALSE, ...) {
  result <- list(
    initial = NULL,
    evaluations = list(),
    refined = list()
  )

  chat <- chat_obj$clone()

  extract_with_retry <- function(extraction_prompt, retry_message = NULL) {
    extracted <- tryCatch(
      {
        chat$chat_structured(extraction_prompt, type = type, ...)
      },
      error = function(e) {
        if (!is.null(retry_message)) {
          cli::cli_alert_warning(retry_message)
        }
        NULL
      }
    )

    if (is.null(extracted) && !is.null(retry_message)) {
      extracted <- tryCatch(
        {
          chat$chat_structured(extraction_prompt, type = type, ...)
        },
        error = function(e) {
          NULL
        }
      )
    }

    return(extracted)
  }

  result$initial <- extract_with_retry(prompt, "Initial extraction failed, retrying...")

  if (is.null(result$initial)) {
    stop("Structured data extraction is NULL. Try again or remove the 'type' argument to check for other errors.")
  }

  current_extraction <- result$initial

  evaluation_rounds <- if (eval) 1 else 0

  if (evaluation_rounds > 0) {
    for (i in 1:evaluation_rounds) {
      tryCatch(
        {
          eval_prompt <- paste(
            "Evaluate my data extraction for flaws and improvements.",
            "I extracted the following structured data:",
            jsonlite::toJSON(current_extraction, pretty = TRUE, auto_unbox = TRUE),
            "The original prompt was:", prompt
          )

          evaluation <- chat$chat(eval_prompt, echo = echo, ...)
          if (!is.null(evaluation)) {
            result$evaluations[[i]] <- evaluation

            refine_prompt <- paste(
              "Extract the following data more accurately:",
              prompt,
              "The prior extraction had the following structured data:",
              jsonlite::toJSON(current_extraction, pretty = TRUE, auto_unbox = TRUE),
              "The prior extraction had these issues:", evaluation
            )

            refined <- extract_with_retry(refine_prompt, paste("Refinement", i, "failed, retrying..."))

            if (!is.null(refined)) {
              result$refined[[i]] <- refined
              current_extraction <- refined
            }
          }
        },
        error = function(e) {
          cli::cli_alert_warning(paste0("Error in evaluation round", i, ": ", conditionMessage(e)))
        }
      )
    }
  }

  result$final <- current_extraction
  result$chat <- chat

  return(result)
}

#' Handle lot interruption
#' @name handle_lot_interrupt
#' @usage handle_lot_interrupt(result, beep)
#' @param result A lot object containing processing state
#' @param beep Logical indicating whether to play a sound
#' @return NULL (called for side effects)
#' @keywords internal
#' @noRd
handle_lot_interrupt <- function(result, beep) {
  cli::cli_alert_warning(
    sprintf(
      "Interrupted at chat %d of %d",
      result@completed, length(result@prompts)
    )
  )
  if (beep) beepr::beep("coin")
}

#' Finish successful lot processing
#' @description Called after successful completion of lot processing to update progress
#'   indicators and provide feedback
#' @param pb Progress bar object
#' @param beep Logical; whether to play success sound
#' @param progress Whether to show progress bars
#' @return NULL (invisibly)
#' @keywords internal
#' @noRd
finish_successful_lot <- function(pb, beep, progress) {
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }
  if (progress) {
    cli::cli_alert_success("Complete")
  }
  if (beep) beepr::beep("ping")
  invisible()
}

#' Create results object from lot
#' @param result Lot object
#' @return Results object with class "lot"
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

  structure(base_list, class = "lot")
}
