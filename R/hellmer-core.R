#' Check if an error is eligible for retry
#' @param error An error object
#' @return TRUE if eligible for retry, FALSE otherwise
#' @keywords internal
is_retry_error <- function(error) {
  retryable_classes <- c(
    "httr2_http_429", "httr2_http_500",
    "httr2_http_503", "httr2_http_504",
    "httr2_http_529"
  )
  for (cls in retryable_classes) {
    if (inherits(error, cls)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Capture chat model response with proper handling
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type_spec Type specification for structured data
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @return List containing response information
#' @keywords internal
capture <- function(original_chat,
                    prompt,
                    type_spec,
                    judgements,
                    echo,
                    ...) {
  response <- NULL
  structured_data <- NULL
  chat <- original_chat$clone()

  result <- withCallingHandlers(
    {
      if (!is.null(type_spec)) {
        result <- process_judgements(chat, prompt, type_spec, judgements, echo = echo, ...)
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

#' Capture chat model response with proper handling and retries
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type_spec Type specification for structured data
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @param max_retries Maximum number of retry attempts
#' @param initial_delay Initial delay in seconds before first retry
#' @param max_delay Maximum delay in seconds between retries
#' @param backoff_factor Factor to multiply delay by after each retry
#' @return List containing response information
#' @keywords internal
capture_with_retry <- function(original_chat,
                               prompt,
                               type_spec,
                               judgements,
                               max_retries,
                               initial_delay,
                               max_delay,
                               backoff_factor,
                               echo,
                               ...) {
  retry_with_delay <- function(attempt = 1, delay = initial_delay) {
    withCallingHandlers(
      tryCatch(
        {
          capture(original_chat, prompt, type_spec, judgements, echo = echo, ...)
        },
        error = function(e) {
          if (!is_retry_error(e)) {
            stop(conditionMessage(e),
              call. = FALSE, domain = "capture_with_retry"
            )
          }

          if (attempt > max_retries) {
            stop(sprintf("Failed after %d attempts. Last error: %s", max_retries, e$message),
              call. = FALSE, domain = "capture_with_retry"
            )
          } else {
            cli::cli_alert_warning(sprintf(
              "Attempt %d failed: %s. Retrying in %.1f seconds...",
              attempt, e$message, delay
            ))

            Sys.sleep(delay)
            next_delay <- min(delay * backoff_factor, max_delay)
            retry_with_delay(attempt + 1, next_delay)
          }
        }
      ),
      interrupt = function(e) {
        signalCondition(e)
      }
    )
  }

  retry_with_delay()
}

#' Process batch of prompts with progress tracking and retries
#' @param chat_obj Chat model object
#' @param prompts List of prompts
#' @param type_spec Type specification for structured data
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @param state_path Path for saving state
#' @param progress Whether to show progress bars
#' @param beep Play sound on completion
#' @param max_retries Maximum retry attempts
#' @param initial_delay Initial delay before retry
#' @param max_delay Maximum delay between retries
#' @param backoff_factor Factor to multiply delay
#' @return Batch results object
#' @keywords internal
process_sequential <- function(
    chat_obj,
    prompts,
    type_spec,
    judgements,
    state_path,
    progress,
    max_retries,
    initial_delay,
    max_delay,
    backoff_factor,
    beep,
    echo,
    ...) {
  if (file.exists(state_path)) {
    result <- readRDS(state_path)
    if (!identical(as.list(prompts), result@prompts)) {
      cli::cli_alert_warning("Prompts don't match saved state. Starting fresh.")
      unlink(state_path)
      result <- NULL
    }
  } else {
    result <- NULL
  }

  if (is.null(result)) {
    orig_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"
    result <- batch(
      prompts = as.list(prompts),
      responses = vector("list", length(prompts)),
      completed = 0L,
      state_path = state_path,
      type_spec = type_spec,
      judgements = as.integer(judgements),
      progress = progress,
      input_type = orig_type,
      max_retries = as.integer(max_retries),
      initial_delay = initial_delay,
      max_delay = max_delay,
      backoff_factor = backoff_factor,
      chunk_size = NULL,
      workers = NULL,
      plan = NULL,
      state = NULL
    )
    saveRDS(result, state_path)
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
        "[{cli::pb_bar}] {cli::pb_percent}"
      ),
      total = total_prompts
    )
    cli::cli_progress_update(id = pb, set = result@completed)
  }

  tryCatch({
    for (i in (result@completed + 1L):total_prompts) {
      response <- capture_with_retry(
        chat_obj, prompts[[i]], type_spec,
        judgements = judgements,
        max_retries = max_retries,
        initial_delay = initial_delay,
        max_delay = max_delay,
        backoff_factor = backoff_factor,
        echo = echo,
        ...
      )

      result@responses[[i]] <- response
      result@completed <- i
      saveRDS(result, state_path)

      if (!is.null(pb)) {
        cli::cli_progress_update(id = pb, set = i)
      }
    }

    finish_successful_batch(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(result, state_path)

    if (inherits(e, "interrupt")) {
      handle_batch_interrupt(result, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(result, state_path)

    if (beep) beepr::beep("coin")
    cli::cli_alert_warning(sprintf(
      "Interrupted at chat %d of %d",
      result@completed, total_prompts
    ))
  }, finally = {
    if (!exists("result")) {
      result <- readRDS(state_path)
    }
  })

  create_results(result)
}

#' Process prompts in parallel chunks with error handling and state management
#' @param chat_obj Chat model object for API calls
#' @param prompts Vector or list of prompts to process
#' @param type_spec Optional type specification for structured data extraction
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @param state_path Path to save intermediate state
#' @param workers Number of parallel workers
#' @param chunk_size Number of prompts to process in parallel at a time
#' @param plan Parallel backend
#' @param beep Play sound on completion/error
#' @param max_chunk_attempts Maximum retries per failed chunk
#' @param max_retries Maximum retries per prompt
#' @param initial_delay Initial delay before first retry
#' @param max_delay Maximum delay between retries
#' @param backoff_factor Delay multiplier after each retry
#' @param progress Whether to show progress bars
#' @return Batch results object
#' @keywords internal
process_future <- function(
    chat_obj,
    prompts,
    type_spec,
    judgements,
    state_path,
    workers,
    chunk_size,
    plan,
    max_chunk_attempts,
    max_retries,
    initial_delay,
    max_delay,
    backoff_factor,
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

  plan <- match.arg(plan, c("multisession", "multicore"))
  total_prompts <- length(prompts)
  prompts_list <- as.list(prompts)
  original_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"

  if (file.exists(state_path)) {
    result <- readRDS(state_path)
    if (!identical(prompts_list, result@prompts)) {
      cli::cli_alert_warning("Prompts don't match saved state. Starting fresh.")
      unlink(state_path)
      result <- NULL
    }
  } else {
    result <- NULL
  }

  if (is.null(result)) {
    result <- batch(
      prompts = prompts_list,
      responses = vector("list", total_prompts),
      completed = 0L,
      state_path = state_path,
      type_spec = type_spec,
      judgements = as.integer(judgements),
      progress = progress,
      input_type = original_type,
      max_retries = max_retries,
      initial_delay = initial_delay,
      max_delay = max_delay,
      backoff_factor = backoff_factor,
      chunk_size = as.integer(chunk_size),
      workers = as.integer(workers),
      plan = plan,
      state = list(
        active_workers = 0L,
        failed_chunks = list(),
        retry_count = 0L
      )
    )
    saveRDS(result, state_path)
  }

  if (result@completed >= total_prompts) {
    if (progress) {
      cli::cli_alert_success("Complete")
    }
    return(create_results(result))
  }

  if (plan == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }

  remaining_prompts <- prompts[(result@completed + 1L):total_prompts]
  chunks <- split(remaining_prompts, ceiling(seq_along(remaining_prompts) / chunk_size))

  pb <- NULL
  if (progress) {
    pb <- cli::cli_progress_bar(
      format = "Processing chats [{cli::pb_current}/{cli::pb_total}] [{cli::pb_bar}] {cli::pb_percent}",
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
                        capture_with_retry(
                          worker_chat,
                          prompt,
                          type_spec,
                          judgements = judgements,
                          max_retries = max_retries,
                          initial_delay = initial_delay,
                          max_delay = max_delay,
                          backoff_factor = backoff_factor,
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
                    # Extract the core error message without the furrr wrapper info
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
                if (!is_retry_error(e)) {
                  stop(conditionMessage(e),
                    call. = FALSE, domain = "process_future"
                  )
                }
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
          saveRDS(result, state_path)
          if (!is.null(pb)) {
            cli::cli_progress_update(id = pb, set = result@completed)
          }
        } else if (retry_count < max_chunk_attempts &&
          !is.null(last_error) && is_retry_error(last_error)) {
          delay <- min(initial_delay * (backoff_factor^(retry_count - 1)), max_delay)
          cli::cli_alert_warning(sprintf(
            "Chunk %d failed (attempt %d/%d): %s. Retrying in %.1f seconds...",
            chunk_idx, retry_count, max_chunk_attempts, validation$message, delay
          ))
          Sys.sleep(delay)
        } else if (!is.null(last_error) && inherits(last_error, "httr2_http")) {
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

    finish_successful_batch(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(result, state_path)

    if (inherits(e, "interrupt")) {
      handle_batch_interrupt(result, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(result, state_path)

    if (beep) beepr::beep("coin")
    cli::cli_alert_warning(sprintf(
      "Interrupted at chat %d of %d",
      result@completed, total_prompts
    ))
  }, finally = {
    if (!exists("result")) {
      result <- readRDS(state_path)
    }
    future::plan(future::sequential)
  })

  create_results(result)
}

#' Process chunks of prompts in parallel
#' @param chunks List of prompt chunks to process
#' @param result A batch object to store results
#' @param chat_obj Chat model object for making API calls
#' @param type_spec Type specification for structured data extraction
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @param pb Progress bar object
#' @param state_path Path to save intermediate state
#' @param progress Whether to show progress bars
#' @param beep Logical indicating whether to play sounds
#' @param max_retries Maximum number of retry attempts
#' @param initial_delay Initial delay in seconds before first retry
#' @param max_delay Maximum delay in seconds between retries
#' @param backoff_factor Factor to multiply delay by after each retry
#' @return Updated batch object with processed results
#' @keywords internal
process_chunks <- function(chunks,
                           result,
                           chat_obj,
                           type_spec,
                           judgements,
                           pb,
                           state_path,
                           progress,
                           beep,
                           max_retries,
                           initial_delay,
                           max_delay,
                           backoff_factor,
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
            capture_with_retry(
              worker_chat,
              prompt,
              type_spec,
              judgements = judgements,
              max_retries = max_retries,
              initial_delay = initial_delay,
              max_delay = max_delay,
              backoff_factor = backoff_factor,
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
        saveRDS(result, state_path)
        if (!is.null(pb)) {
          cli::cli_progress_update(id = pb, set = end_idx)
        }
      },
      interrupt = function(e) {
        was_interrupted <<- TRUE
        handle_batch_interrupt(result, beep)
        invokeRestart("abort")
      }
    )
  }

  if (!was_interrupted) {
    finish_successful_batch(pb, beep, progress)
  }
}

#' Process structured data extraction with judgement
#' @param chat_obj Chat model object
#' @param prompt The prompt or text to analyze
#' @param type_spec Type specification for structured data
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @return List containing extraction process
#' @keywords internal
process_judgements <- function(chat_obj, prompt, type_spec, judgements = 0, echo = FALSE, ...) {
  result <- list(
    initial = NULL,
    evaluations = list(),
    refined = list()
  )

  chat <- chat_obj$clone()

  result$initial <- chat$extract_data(
    prompt,
    type = type_spec,
    echo = echo,
    ...
  )

  current_extraction <- result$initial

  judgement_rounds <- judgements

  if (judgement_rounds > 0) {
    for (i in 1:judgement_rounds) {
      eval_prompt <- paste(
        "What could be improved in my data extraction?",
        "I extracted the following structured data:",
        jsonlite::toJSON(current_extraction, pretty = TRUE, auto_unbox = TRUE),
        "The original prompt was:", prompt
      )

      evaluation <- chat$chat(eval_prompt, echo = echo, ...)
      result$evaluations[[i]] <- evaluation

      refine_prompt <- paste(
        "Extract the following data more accurately:",
        prompt,
        "The prior extraction had the following structured data:",
        jsonlite::toJSON(current_extraction, pretty = TRUE, auto_unbox = TRUE),
        "The prior extraction had these issues:", evaluation
      )

      refined <- chat$extract_data(
        refine_prompt,
        type = type_spec,
        echo = echo,
        ...
      )

      result$refined[[i]] <- refined
      current_extraction <- refined
    }
  }

  result$final <- current_extraction
  result$chat <- chat

  return(result)
}

#' Handle batch interruption
#' @name handle_batch_interrupt
#' @usage handle_batch_interrupt(result, beep)
#' @param result A batch object containing processing state
#' @param beep Logical indicating whether to play a sound
#' @return NULL (called for side effects)
#' @keywords internal
handle_batch_interrupt <- function(result, beep) {
  cli::cli_alert_warning(
    sprintf(
      "Interrupted at chat %d of %d",
      result@completed, length(result@prompts)
    )
  )
  if (beep) beepr::beep("coin")
}

#' Finish successful batch processing
#' @description Called after successful completion of batch processing to update progress
#'   indicators and provide feedback
#' @param pb Progress bar object
#' @param beep Logical; whether to play success sound
#' @param progress Whether to show progress bars
#' @return NULL (invisibly)
#' @keywords internal
finish_successful_batch <- function(pb, beep, progress) {
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }
  if (progress) {
    cli::cli_alert_success("Complete")
  }
  if (beep) beepr::beep("ping")
  invisible()
}

#' Create results object from batch
#' @param result Batch object
#' @return Results object with class "batch"
#' @keywords internal
create_results <- function(result) {
  base_list <- list(
    prompts = result@prompts,
    responses = result@responses,
    completed = result@completed,
    state_path = result@state_path,
    type_spec = result@type_spec
  )

  base_list$texts <- function() texts(result)
  base_list$chats <- function() chats(result)
  base_list$progress <- function() progress(result)

  structure(base_list, class = "batch")
}
