#' Check if an error is an authentication error
#' @param error Error message or condition
#' @return TRUE if authentication error, FALSE otherwise
#' @keywords internal
is_auth_error <- function(error) {
  msg <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error)
  grepl("unauthorized|authentication|invalid.*key|api.*key",
    tolower(msg),
    ignore.case = TRUE
  )
}

#' Create a standardized authentication error
#' @param original_error Original error message or condition
#' @return Structured error information
#' @keywords internal
create_auth_error <- function(original_error) {
  msg <- if (inherits(original_error, "condition")) {
    conditionMessage(original_error)
  } else {
    as.character(original_error)
  }

  structure(
    list(
      success = FALSE,
      error = "auth",
      message = msg
    ),
    class = c("chat_auth_error", "error", "condition")
  )
}

#' Capture chat model response with proper handling
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type_spec Type specification for structured data
#' @param echo Echo level ("none", "text", "all")
#' @return List containing response information
#' @keywords internal
capture <- function(original_chat, prompt, type_spec = NULL, echo = "text") {
  response <- NULL
  structured_data <- NULL
  chat <- original_chat$clone()

  if (echo == "all") {
    cli::cli_h3(cli::col_green("Prompt"))
    cli::cli_text(prompt)
    cli::cli_h3(cli::col_green("Response"))
  }

  if (!is.null(type_spec)) {
    if (echo == "none") {
      temp_file <- tempfile()
      sink(temp_file)
      structured_data <- chat$extract_data(prompt, type = type_spec)
      sink()
      unlink(temp_file)
    } else {
      structured_data <- chat$extract_data(prompt, type = type_spec)
      str(structured_data)
    }

    if (is.null(structured_data)) {
      stop("Received NULL structured data response")
    }
  } else {
    if (echo == "none") {
      response <- chat$chat(prompt)
    } else {
      response <- chat$chat(prompt)
      cli::cat_line(response)
    }

    if (is.null(response)) {
      stop("Received NULL chat response")
    }
  }

  turns <- chat$get_turns()
  tokens <- chat$tokens()

  if (is.null(turns) || length(turns) == 0) {
    stop("No chat turns recorded")
  }

  if (is.null(tokens)) {
    stop("No token information available")
  }

  list(
    chat = chat,
    text = response,
    structured_data = structured_data,
    turns = turns,
    tokens = tokens
  )
}

#' Capture chat model response with proper handling and retries
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type_spec Type specification for structured data
#' @param echo Echo level ("none", "text", "all")
#' @param max_retries Maximum number of retry attempts
#' @param initial_delay Initial delay in seconds before first retry
#' @param max_delay Maximum delay in seconds between retries
#' @param backoff_factor Factor to multiply delay by after each retry
#' @return List containing response information
#' @keywords internal
capture_with_retry <- function(original_chat, prompt, type_spec = NULL,
                               echo = "text", max_retries = 3L,
                               initial_delay = 1, max_delay = 32,
                               backoff_factor = 2, timeout = 60) {
  retry_with_delay <- function(attempt = 1, delay = initial_delay) {
    tryCatch(
      {
        R.utils::withTimeout(
          {
            capture(original_chat, prompt, type_spec, echo)
          },
          timeout = timeout
        )
      },
      error = function(e) {
        if (inherits(e, "interrupt")) {
          stop(e)
        }

        if (is_auth_error(e)) {
          stop(create_auth_error(e)$message)
        }

        if (attempt > max_retries) {
          structure(
            list(
              message = sprintf(
                "Failed after %d attempts. Last error: %s",
                max_retries, e$message
              )
            ),
            class = c("chat_error", "error", "condition")
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
      },
      timeout = function(e) {
        structure(
          list(message = sprintf("Operation timed out after %d seconds", timeout)),
          class = c("chat_error", "error", "condition")
        )
      }
    )
  }

  retry_with_delay()
}

#' Process batch of prompts with progress tracking and retries
#' @param chat_obj Chat model object
#' @param prompts List of prompts
#' @param type_spec Type specification for structured data
#' @param state_path Path for saving state
#' @param echo Echo level ("none", "text", or "all")
#' @param beep Play sound on completion, interruption, and error
#' @param max_retries Maximum number of retry attempts per prompt
#' @param initial_delay Initial delay in seconds before first retry
#' @param max_delay Maximum delay in seconds between retries
#' @param backoff_factor Factor to multiply delay by after each retry
#' @param timeout Maximum time in seconds to wait for each prompt response
#' @return Batch results object
#' @keywords internal
process <- function(
    chat_obj,
    prompts,
    type_spec = NULL,
    state_path = tempfile("chat_", fileext = ".rds"),
    echo = "none",
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 60,
    backoff_factor = 2,
    timeout = 60,
    beep = TRUE) {
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
      echo = echo,
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
    cli::cli_alert_success("Complete")
    return(create_results(result))
  }

  if (echo == "none") {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Processing chats [{cli::pb_current}/{cli::pb_total}] ",
        "[{cli::pb_bar}] {cli::pb_percent}"
      ),
      total = total_prompts
    )
    cli::cli_progress_update(id = pb, set = result@completed)
  } else {
    pb <- NULL
  }

  tryCatch({
    for (i in (result@completed + 1L):total_prompts) {
      if (echo %in% c("text", "all")) {
        cli::cli_h3(cli::col_green(sprintf("Processing chats [%d/%d]", i, total_prompts)))
      }

      response <- capture_with_retry(
        chat_obj, prompts[[i]], type_spec, echo,
        max_retries = max_retries,
        initial_delay = initial_delay,
        max_delay = max_delay,
        backoff_factor = backoff_factor,
        timeout = timeout
      )

      result@responses[[i]] <- response
      result@completed <- i
      saveRDS(result, state_path)

      if (!is.null(pb)) {
        cli::cli_progress_update(id = pb, set = i)
      }
    }

    finish_successful_batch(pb, beep)
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
#' @param state_path Path to save intermediate state
#' @param workers Number of parallel workers (default: number of CPU cores)
#' @param chunk_size Number of prompts to process in parallel at a time (default: 10% of the number of prompts)
#' @param plan Parallel backend: "multisession" or "multicore"
#' @param beep Play sound on completion/error (default: TRUE)
#' @param timeout Maximum seconds per prompt (default: 60)
#' @param max_chunk_attempts Maximum retries per failed chunk (default: 3)
#' @param max_retries Maximum retries per prompt (default: 3)
#' @param initial_delay Initial delay before first retry (default: 1)
#' @param max_delay Maximum delay between retries (default: 32)
#' @param backoff_factor Delay multiplier after each retry (default: 2)
#' @return Batch results object containing processed responses
#' @keywords internal
process_future <- function(
    chat_obj,
    prompts,
    type_spec = NULL,
    state_path = tempfile("chat_", fileext = ".rds"),
    workers = parallel::detectCores(),
    chunk_size = NULL,
    plan = "multisession",
    max_chunk_attempts = 3L,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 60,
    backoff_factor = 2,
    timeout = 60,
    beep = TRUE) {
  validate_chunk_result <- function(chunk_result, chunk_idx) {
    if (inherits(chunk_result, "error") || inherits(chunk_result, "worker_error")) {
      if (is_auth_error(chunk_result)) {
        stop(create_auth_error(chunk_result)$message)
      }
      return(list(valid = FALSE, message = conditionMessage(chunk_result)))
    }

    if (!is.list(chunk_result) || !("responses" %in% names(chunk_result))) {
      return(list(valid = FALSE, message = sprintf("Invalid chunk structure in chunk %d", chunk_idx)))
    }

    if (length(chunk_result$responses) == 0) {
      return(list(valid = FALSE, message = sprintf("Empty responses in chunk %d", chunk_idx)))
    }

    list(valid = TRUE, message = NULL)
  }

  plan <- match.arg(plan, c("multisession", "multicore"))
  total_prompts <- length(prompts)
  prompts_list <- as.list(prompts)
  original_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"

  if (is.null(chunk_size)) {
    chunk_size <- max(1L, ceiling(length(prompts) / 10))
    cli::cli_alert_info("Defaulting to {.field chunk_size} of {.val {chunk_size}} prompts per chunk")
  }

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
      echo = "none",
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
        retry_count = 0L,
        tokens = list()
      )
    )
    saveRDS(result, state_path)
  }

  if (result@completed >= total_prompts) {
    cli::cli_alert_success("Complete")
    return(create_results(result))
  }

  if (plan == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }

  remaining_prompts <- prompts[(result@completed + 1L):total_prompts]
  chunks <- split(remaining_prompts, ceiling(seq_along(remaining_prompts) / chunk_size))

  pb <- cli::cli_progress_bar(
    format = "Processing chats [{cli::pb_current}/{cli::pb_total}] [{cli::pb_bar}] {cli::pb_percent}",
    total = total_prompts
  )
  cli::cli_progress_update(id = pb, set = result@completed)

  tryCatch({
    for (chunk_idx in seq_along(chunks)) {
      chunk <- chunks[[chunk_idx]]
      retry_count <- 0
      success <- FALSE
      last_error <- NULL

      while (!success && retry_count < max_chunk_attempts) {
        retry_count <- retry_count + 1
        worker_chat <- chat_obj$clone()

        chunk_result <- tryCatch(
          {
            responses <- furrr::future_map(
              chunk,
              function(prompt) {
                capture_with_retry(
                  worker_chat,
                  prompt,
                  type_spec,
                  echo = "none",
                  timeout = timeout,
                  max_retries = max_retries,
                  initial_delay = initial_delay,
                  max_delay = max_delay,
                  backoff_factor = backoff_factor
                )
              },
              .options = furrr::furrr_options(
                scheduling = 1,
                seed = TRUE
              )
            )

            list(
              success = TRUE,
              responses = responses,
              tokens = worker_chat$tokens()
            )
          },
          error = function(e) {
            last_error <- e
            structure(
              list(
                success = FALSE,
                error = if (is_auth_error(e)) "auth" else "other",
                message = conditionMessage(e)
              ),
              class = c("worker_error", "error", "condition")
            )
          }
        )

        validation <- validate_chunk_result(chunk_result, chunk_idx)
        success <- validation$valid

        if (success) {
          start_idx <- result@completed + 1
          end_idx <- result@completed + length(chunk)

          result@responses[start_idx:end_idx] <- chunk_result$responses
          if (!is.null(chunk_result$tokens)) {
            result@state$tokens[[chunk_idx]] <- chunk_result$tokens
          }

          result@completed <- end_idx
          saveRDS(result, state_path)
          cli::cli_progress_update(id = pb, set = result@completed)
        } else if (retry_count < max_chunk_attempts) {
          cli::cli_alert_warning(sprintf(
            "Chunk %d failed (attempt %d/%d): %s. Retrying...",
            chunk_idx, retry_count, max_chunk_attempts, validation$message
          ))
          Sys.sleep(2^retry_count)
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

    finish_successful_batch(pb, beep)
  }, error = function(e) {
    cli::cli_progress_done(id = pb)
    saveRDS(result, state_path)

    if (inherits(e, "interrupt")) {
      handle_batch_interrupt(result, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    cli::cli_progress_done(id = pb)
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
#' @param pb Progress bar object
#' @param state_path Path to save intermediate state
#' @param echo Level of output to display ("none", "text", "all")
#' @param beep Logical indicating whether to play sounds
#' @return Updated batch object with processed results
#' @keywords internal
process_chunks <- function(chunks, result, chat_obj, type_spec, pb, state_path, echo, beep) {
  was_interrupted <- FALSE

  for (chunk in chunks) {
    if (was_interrupted) break

    withCallingHandlers(
      {
        new_responses <- furrr::future_map(
          chunk,
          function(prompt) {
            worker_chat <- chat_obj$clone()
            capture_with_retry(worker_chat, prompt, type_spec, echo = "none")
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
    finish_successful_batch(pb, beep)
  }
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
#' @return NULL (invisibly)
#' @keywords internal
finish_successful_batch <- function(pb, beep) {
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }
  cli::cli_alert_success("Complete")
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
  base_list$structured_data <- function() structured_data(result)

  structure(base_list, class = "batch")
}
