#' @export
texts <- S7::new_generic("texts", "x")

#' @export
chats <- S7::new_generic("chats", "x")

#' @export
progress <- S7::new_generic("progress", "x")

#' @export
structured_data <- S7::new_generic("structured_data", "x")

#' @import ellmer
#' @import S7
#' @importFrom cli cli_alert_success cli_alert_warning cli_h3 cli_text col_green
#' @importFrom cli cli_progress_bar cli_progress_done
#' @importFrom furrr future_map
#' @importFrom future plan multisession
#' @importFrom purrr map_lgl pwalk map map_chr walk
#' @importFrom R.utils withTimeout
#' @keywords internal
"_PACKAGE"

#' Batch class for managing chat processing
#' @name batch
#' @export
batch <- S7::new_class(
  "batch",
  properties = list(
    prompts = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        if (length(value) == 0) {
          "@prompts must not be empty"
        }
        if (!all(purrr::map_lgl(value, is.character))) {
          "@prompts must be a list of character strings"
        }
        NULL
      }
    ),
    responses = S7::new_property(
      class = S7::class_list,
      validator = function(value) NULL
    ),
    completed = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          "@completed must be a single integer"
        }
        if (value < 0) {
          "@completed must be non-negative"
        }
        NULL
      }
    ),
    state_path = S7::class_character | NULL,
    type_spec = S7::new_property(
      class = S7::class_any | NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!inherits(value, c("ellmer::TypeObject", "ellmer::Type", "ellmer::TypeArray"))) {
            return("@type_spec must be an ellmer type specification (created with type_object(), type_array(), etc.) or NULL")
          }
        }
        NULL
      }
    ),
    echo = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (!value %in% c("none", "text", "all")) {
          "@echo must be one of 'none', 'text', or 'all'"
        }
        NULL
      }
    ),
    progress_bar = S7::new_property(
      class = S7::class_any | NULL
    ),
    input_type = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (!value %in% c("vector", "list")) {
          "input_type must be either 'vector' or 'list'"
        } else {
          NULL
        }
      }
    ),
    max_retries = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          "@max_retries must be a single integer"
        }
        if (value < 0) {
          "@max_retries must be non-negative"
        }
        NULL
      }
    ),
    initial_delay = S7::new_property(
      class = S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1) {
          "@initial_delay must be a single numeric"
        }
        if (value < 0) {
          "@initial_delay must be non-negative"
        }
        NULL
      }
    ),
    max_delay = S7::new_property(
      class = S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1) {
          "@max_delay must be a single numeric"
        }
        if (value < 0) {
          "@max_delay must be non-negative"
        }
        NULL
      }
    ),
    backoff_factor = S7::new_property(
      class = S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1) {
          "@backoff_factor must be a single numeric"
        }
        if (value <= 1) {
          "@backoff_factor must be greater than 1"
        }
        NULL
      }
    ),
    chunk_size = S7::new_property(
      class = S7::class_integer | NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "@chunk_size must be a single integer"
          }
          if (value <= 0) {
            "@chunk_size must be positive"
          }
        }
        NULL
      }
    ),
    workers = S7::new_property(
      class = S7::class_integer | NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "@workers must be a single integer"
          }
          if (value <= 0) {
            "@workers must be positive"
          }
        }
        NULL
      }
    ),
    plan = S7::new_property(
      class = S7::class_character | NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!value %in% c("multisession", "multicore")) {
            "@plan must be either 'multisession' or 'multicore'"
          }
        }
        NULL
      }
    ),
    state = S7::new_property(
      class = S7::class_list | NULL,
      validator = function(value) {
        if (!is.null(value)) {
          required_fields <- c("active_workers", "failed_chunks", "retry_count")
          if (!all(required_fields %in% names(value))) {
            return("@state must contain active_workers, failed_chunks, and retry_count")
          }
        }
        NULL
      }
    )
  ),
  validator = function(self) {
    if (self@completed > length(self@prompts)) {
      "@completed cannot be larger than number of prompts"
    }
    if (!is.null(self@state)) {
      if (self@state$active_workers > self@workers) {
        return("Active workers cannot exceed total workers")
      }
    }
    NULL
  }
)

#' Extract structured data from a batch
#' @name structured_data.batch
#' @param x A batch object
#' @return List of structured data
#' @importFrom cli cli_alert_warning
#' @importFrom purrr map
S7::method(structured_data, batch) <- function(x) {
  if (is.null(x@type_spec)) {
    cli_alert_warning("No type specification provided for structured data extraction")
    return(NULL)
  }
  responses <- x@responses[seq_len(x@completed)]
  map(responses, "structured_data")
}

#' Extract text responses from a batch
#' @name texts.batch
#' @param x A batch object
#' @return A character vector (if original prompts were supplied as a vector) or
#'   a list of response texts (if original prompts were supplied as a list)
#' @importFrom purrr map_chr
S7::method(texts, batch) <- function(x) {
  responses <- x@responses[seq_len(x@completed)]
  text_values <- purrr::map_chr(responses, "text")

  if (x@input_type == "vector") {
    return(text_values)
  } else {
    return(as.list(text_values))
  }
}

#' Extract chat objects from a batch
#' @name chats.batch
#' @param x A batch object
#' @return List of chat objects
#' @importFrom purrr map
S7::method(chats, batch) <- function(x) {
  responses <- x@responses[seq_len(x@completed)]
  map(responses, "chat")
}

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
  } else {
    if (echo == "none") {
      response <- chat$chat(prompt)
    } else {
      response <- chat$chat(prompt)
      cat(response, "\n")
    }
  }

  list(
    chat = chat,
    text = response,
    structured_data = structured_data,
    turns = chat$get_turns(),
    tokens = chat$tokens()
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
    result <- tryCatch(
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
          stop(e$message)
        }

        cli::cli_alert_warning(sprintf(
          "Attempt %d failed: %s. Retrying in %.1f seconds...",
          attempt, e$message, delay
        ))

        Sys.sleep(delay)
        next_delay <- min(delay * backoff_factor, max_delay)
        retry_with_delay(attempt + 1, next_delay)
      },
      timeout = function(e) {
        stop(sprintf("Operation timed out after %d seconds", timeout))
      }
    )

    result
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
    state_path = tempfile("chat_batch_", fileext = ".rds"),
    echo = "text",
    beep = TRUE,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
    timeout = 60) {
  if (file.exists(state_path)) {
    result <- readRDS(state_path)
    if (!identical(as.list(prompts), result@prompts)) {
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
  }

  total_prompts <- length(prompts)

  if (result@completed >= total_prompts) {
    if (echo == "none") {
      cli::cli_alert_success("Complete")
    }
    return(create_results(result))
  }

  if (echo == "none") {
    progress_bar <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Processing chats [{cli::pb_current}/{cli::pb_total}] ",
        "[{cli::pb_bar}] {cli::pb_percent}"
      ),
      total = total_prompts
    )
    cli::cli_progress_update(id = progress_bar, set = result@completed)
  }

  remaining_chats <- (result@completed + 1L):total_prompts

  tryCatch(
    {
      for (i in remaining_chats) {
        if (echo %in% c("text", "all")) {
          cli::cli_h3(cli::col_green(sprintf("Processing chats [%d/%d]", i, total_prompts)))
        }

        response <- capture_with_retry(chat_obj, prompts[[i]], type_spec, echo,
          max_retries = max_retries, initial_delay = initial_delay,
          max_delay = max_delay, backoff_factor = backoff_factor,
          timeout = timeout
        )
        result@responses[[i]] <- response
        result@completed <- i
        saveRDS(result, state_path)

        if (echo == "none") {
          cli::cli_progress_update(id = progress_bar, set = i)
        }
      }

      cli::cli_alert_success("Complete")
      if (echo == "none") {
        cli::cli_progress_done(id = progress_bar)
      }

      if (beep) {
        beepr::beep("ping")
      }
    },
    error = function(e) {
      saveRDS(result, state_path)
      if (echo == "none") {
        cli::cli_progress_done(id = progress_bar)
      }
      if (beep) {
        beepr::beep("wilhelm")
      }
      cli::cli_alert_warning(
        paste0("Interrupted at chat ", result@completed, " of ", total_prompts)
      )
      if (!inherits(e, "interrupt")) {
        stop(e)
      }
    },
    interrupt = function(e) {
      saveRDS(result, state_path)
      if (echo == "none") {
        cli::cli_progress_done(id = progress_bar)
      }
      if (beep) {
        beepr::beep("coin")
      }
      cli::cli_alert_warning(
        paste0("Interrupted at chat ", result@completed, " of ", total_prompts)
      )
    }
  )

  create_results(result)
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
    state_path = tempfile("chat_batch_", fileext = ".rds"),
    echo = "none",
    beep = TRUE,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
    timeout = 60) {
  if (file.exists(state_path)) {
    result <- readRDS(state_path)
    if (!identical(as.list(prompts), result@prompts)) {
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
  }

  total_prompts <- length(prompts)

  if (result@completed >= total_prompts) {
    if (echo == "none") {
      cli::cli_alert_success("Complete")
    }
    return(create_results(result))
  }

  if (echo == "none") {
    progress_bar <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Processing chats [{cli::pb_current}/{cli::pb_total}] ",
        "[{cli::pb_bar}] {cli::pb_percent}"
      ),
      total = total_prompts
    )
    cli::cli_progress_update(id = progress_bar, set = result@completed)
  }

  remaining_chats <- (result@completed + 1L):total_prompts

  tryCatch(
    {
      for (i in remaining_chats) {
        if (echo %in% c("text", "all")) {
          cli::cli_h3(cli::col_green(sprintf("Processing chats [%d/%d]", i, total_prompts)))
        }

        response <- capture_with_retry(chat_obj, prompts[[i]], type_spec, echo,
          max_retries = max_retries, initial_delay = initial_delay,
          max_delay = max_delay, backoff_factor = backoff_factor,
          timeout = timeout
        )
        result@responses[[i]] <- response
        result@completed <- i
        saveRDS(result, state_path)

        if (echo == "none") {
          cli::cli_progress_update(id = progress_bar, set = i)
        }
      }

      cli::cli_alert_success("Complete")
      if (echo == "none") {
        cli::cli_progress_done(id = progress_bar)
      }

      if (beep) {
        beepr::beep("ping")
      }
    },
    error = function(e) {
      saveRDS(result, state_path)
      if (echo == "none") {
        cli::cli_progress_done(id = progress_bar)
      }
      if (beep) {
        beepr::beep("wilhelm")
      }
      cli::cli_alert_warning(
        paste0("Interrupted at chat ", result@completed, " of ", total_prompts)
      )
      if (!inherits(e, "interrupt")) {
        stop(e)
      }
    },
    interrupt = function(e) {
      saveRDS(result, state_path)
      if (echo == "none") {
        cli::cli_progress_done(id = progress_bar)
      }
      if (beep) {
        beepr::beep("coin")
      }
      cli::cli_alert_warning(
        paste0("Interrupted at chat ", result@completed, " of ", total_prompts)
      )
    }
  )

  create_results(result)
}

#' Process a batch of prompts in parallel
#' @param chat_obj Chat model object for making API calls
#' @param prompts Vector or list of prompts to process
#' @param type_spec Type specification for structured data extraction
#' @param state_path Path to save intermediate state
#' @param workers Number of parallel workers
#' @param chunk_size Number of prompts to process per chunk
#' @param plan Parallel backend: "multisession" or "multicore"
#' @param beep Play sound on completion, interruption, and error
#' @param timeout Maximum time in seconds to wait for each prompt response
#' @param max_chunk_attempts Maximum number of retry attempts for failed chunks
#' @return Batch results object
#' @keywords internal
process_parallel <- function(
    chat_obj,
    prompts,
    type_spec = NULL,
    state_path = tempfile("chat_batch_", fileext = ".rds"),
    workers = 4,
    chunk_size = 4,
    plan = "multisession",
    beep = TRUE,
    timeout = 60,
    max_chunk_attempts = 3L) {
  
  plan <- match.arg(plan, c("multisession", "multicore"))
  total_prompts <- length(prompts)
  prompts_list <- as.list(prompts)
  original_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"
  
  result <- if (file.exists(state_path)) {
    existing_result <- readRDS(state_path)
    prompts_match <- isTRUE(all.equal(
      prompts_list,
      existing_result@prompts,
      check.attributes = FALSE
    ))
    
    if (!prompts_match || existing_result@input_type != original_type) {
      unlink(state_path)
      NULL
    } else {
      existing_result
    }
  } else {
    NULL
  }
  
  if (is.null(result)) {
    result <- batch(
      prompts = prompts_list,
      responses = vector("list", total_prompts),
      completed = 0L,
      state_path = state_path,
      type_spec = type_spec,
      echo = "none",
      progress_bar = NULL,
      input_type = original_type,
      max_retries = 3L,
      initial_delay = 1,
      max_delay = 32,
      backoff_factor = 2,
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
  
  handle_worker_response <- function(response, chunk_idx) {
    if (inherits(response, "error")) {
      if (is_auth_error(response)) {
        stop(create_auth_error(response)$message)
      }
      return(FALSE)
    }
    
    if (!is.null(response$tokens)) {
      result@state$tokens[[chunk_idx]] <- response$tokens
    }
    
    TRUE
  }
  
  if (result@completed < total_prompts) {
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
        
        while (!success && retry_count < max_chunk_attempts) {
          retry_count <- retry_count + 1
          worker_chat <- chat_obj$clone()
          
          chunk_result <- tryCatch({
            responses <- furrr::future_map(
              chunk,
              function(prompt) {
                capture_with_retry(
                  worker_chat,
                  prompt,
                  type_spec,
                  echo = "none",
                  timeout = timeout
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
          }, error = function(e) {
            structure(
              list(
                success = FALSE,
                error = if (is_auth_error(e)) "auth" else "other",
                message = conditionMessage(e)
              ),
              class = c("worker_error", "error", "condition")
            )
          })
          
          success <- handle_worker_response(chunk_result, chunk_idx)
          
          if (!success && retry_count < max_chunk_attempts) {
            cli::cli_alert_warning(sprintf(
              "Chunk %d failed (attempt %d/%d). Retrying...",
              chunk_idx, retry_count, max_chunk_attempts
            ))
            Sys.sleep(2^retry_count)
          }
        }
        
        if (!success) {
          stop(sprintf("Chunk %d failed after %d attempts", chunk_idx, max_chunk_attempts))
        }
        
        result@completed <- result@completed + length(chunk)
        saveRDS(result, state_path)
        cli::cli_progress_update(id = pb, set = result@completed)
      }
      
      cli::cli_progress_done(id = pb)
      if (beep) beepr::beep("ping")
      
    }, error = function(e) {
      cli::cli_progress_done(id = pb)
      saveRDS(result, state_path)
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }, interrupt = function(e) {
      cli::cli_progress_done(id = pb)
      saveRDS(result, state_path)
      if (beep) beepr::beep("coin")
      message(sprintf("\nInterrupted at chat %d of %d", result@completed, total_prompts))
    })
  }
  
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

        update_batch_state(result, new_responses, pb, state_path)
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

#' Update batch state with new responses
#' @param x A batch object

#' Handle batch interruption
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

#' Process a batch of prompts sequentially
#' @description
#' Processes a batch of chat prompts one at a time in sequential order.
#' Maintains state between runs and can resume interrupted processing.
#' For parallel processing, use `chat_parallel()`.
#'
#' @param chat_model Chat model object to use for processing
#' @param echo Level of output to display: "none" for silent operation,
#'        "text" for response text only, or "all" for full interaction
#' @param beep Logical indicating whether to play a sound when batch processing completes
#' @param timeout Maximum time in seconds to wait for each prompt response
#' @param max_retries Maximum number of retry attempts per prompt
#' @param initial_delay Initial delay in seconds before first retry
#' @param max_delay Maximum delay in seconds between retries
#' @param backoff_factor Factor to multiply delay by after each retry
#' @param ... Additional arguments passed to the underlying chat model
#' @return A batch results object containing:
#'   \itemize{
#'     \item prompts: Original input prompts
#'     \item responses: Raw response data for completed prompts
#'     \item completed: Number of successfully processed prompts
#'     \item state_path: Path where batch state is saved
#'     \item type_spec: Type specification used for structured data
#'     \item texts(): Function to extract text responses
#'     \item chats(): Function to extract chat objects
#'     \item progress(): Function to get processing status
#'     \item structured_data(): Function to extract structured data if type_spec was provided
#'   }
#' @export
chat_batch <- function(
    chat_model = ellmer::chat_claude(),
    echo = "none",
    beep = TRUE,
    timeout = 60,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
    ...) {
  chat_env <- new.env(parent = emptyenv())

  chat_env$chat_model <- chat_model
  chat_env$echo <- echo
  chat_env$beep <- beep
  chat_env$timeout <- timeout
  chat_env$max_retries <- max_retries
  chat_env$initial_delay <- initial_delay
  chat_env$max_delay <- max_delay
  chat_env$backoff_factor <- backoff_factor

  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds")) {
    process(
      chat_obj = chat_env$chat_model,
      prompts = prompts,
      type_spec = type_spec,
      state_path = state_path,
      echo = chat_env$echo,
      beep = chat_env$beep,
      max_retries = chat_env$max_retries,
      initial_delay = chat_env$initial_delay,
      max_delay = chat_env$max_delay,
      backoff_factor = chat_env$backoff_factor,
      timeout = chat_env$timeout
    )
  }

  chat_env$chat <- chat_model$chat
  chat_env$clone <- chat_model$clone
  chat_env$extract_data <- chat_model$extract_data
  chat_env$get_turns <- chat_model$get_turns
  chat_env$tokens <- chat_model$tokens

  structure(chat_env, class = class(chat_model))
}

#' Process a batch of prompts in parallel
#' @description
#' Processes a batch of chat prompts using parallel workers.
#' Splits prompts into chunks for processing while maintaining state.
#' For sequential processing, use `chat_batch()`.
#'
#' @param chat_model A chat model object
#' @param workers Number of parallel workers to use
#' @param plan Processing strategy to use: "multisession" for separate R sessions
#'        or "multicore" for forked processes
#' @param beep Logical indicating whether to play sound on completion
#' @param timeout Maximum time in seconds to wait for each prompt response
#' @param max_chunk_attempts Maximum number of retry attempts for failed chunks (default: 3)
#' @param ... Additional arguments passed to the chat model
#' @return A batch results object containing:
#'   \itemize{
#'     \item prompts: Original input prompts
#'     \item responses: Raw response data for completed prompts
#'     \item completed: Number of successfully processed prompts
#'     \item state_path: Path where batch state is saved
#'     \item type_spec: Type specification used for structured data
#'     \item texts(): Function to extract text responses
#'     \item chats(): Function to extract chat objects
#'     \item progress(): Function to get processing status
#'     \item structured_data(): Function to extract structured data if type_spec was provided
#'   }
#' @export
chat_parallel <- function(
    chat_model = ellmer::chat_claude(),
    workers = 4,
    plan = "multisession",
    beep = TRUE,
    timeout = 60,
    max_chunk_attempts = 3L,
    ...) {
  
  plan <- match.arg(plan, choices = c("multisession", "multicore"))
  original_chat <- chat_model
  chat_env <- new.env(parent = emptyenv())
  
  purrr::walk(names(original_chat), function(n) {
    assign(n, original_chat[[n]], envir = chat_env)
  })
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds"),
                             chunk_size = 4) {
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }
    
    process_parallel(
      chat_obj = original_chat,
      prompts = prompts,
      type_spec = type_spec,
      state_path = state_path,
      workers = workers,
      chunk_size = chunk_size,
      plan = plan,
      beep = beep,
      timeout = timeout,
      max_chunk_attempts = max_chunk_attempts
    )
  }
  
  structure(chat_env, class = class(original_chat))
}

#' Extract progress information from a batch
#' @name progress.batch
#' @param x A batch object
#' @return A list containing progress details
#' @importFrom cli cli_alert_warning
S7::method(progress, batch) <- function(x) {
  list(
    total_prompts = length(x@prompts),
    completed_prompts = x@completed,
    completion_percentage = (x@completed / length(x@prompts)) * 100,
    remaining_prompts = length(x@prompts) - x@completed,
    state_path = x@state_path
  )
}
