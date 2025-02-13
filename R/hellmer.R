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
    output <- utils::capture.output(response <- chat$chat(prompt))
    if (echo %in% c("all", "text")) {
      cli::cli_text(paste(output, collapse = "\n"))
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
capture_with_retry <- function(original_chat, prompt, type_spec = NULL, echo = "text",
                               max_retries = 3L, initial_delay = 1, max_delay = 32,
                               backoff_factor = 2) {
  retry_with_delay <- function(attempt = 1, delay = initial_delay) {
    if (attempt > max_retries + 1) {
      stop("Warning 1:")
    }

    result <- tryCatch(
      {
        R.utils::withTimeout(
          {
            capture(original_chat, prompt, type_spec, echo)
          },
          timeout = 60
        )
      },
      error = function(e) {
        if (inherits(e, "interrupt")) {
          stop(e)
        }

        if (grepl("unauthorized|authentication|invalid.*key|api.*key", tolower(e$message))) {
          stop("Authentication error - invalid API key: ", e$message)
        }

        if (attempt > max_retries) {
          stop("Warning 2: ", e$message)
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
        stop("Operation timed out after 60 seconds")
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
#' @param max_retries Maximum number of retry attempts per prompt
#' @param initial_delay Initial delay in seconds before first retry
#' @param max_delay Maximum delay in seconds between retries
#' @param backoff_factor Factor to multiply delay by after each retry
#' @return Batch results object
#' @keywords internal
process <- function(chat_obj, prompts, type_spec = NULL,
                    state_path = tempfile("chat_batch_", fileext = ".rds"),
                    echo = "text", max_retries = 3L, initial_delay = 1,
                    max_delay = 32, backoff_factor = 2,
                    beep = TRUE) {
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

        response <- capture_with_retry(chat_obj, prompts[[i]], type_spec, echo)
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

#' Process batch of prompts in parallel
#' @description Processes a batch of chat prompts using parallel workers.
#' @inheritParams process_parallel
#' @keywords internal
process_parallel <- function(chat_obj, prompts, type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds"),
                             chunk_size = 4, workers = 4, beep = TRUE,
                             plan = "multisession") {
  plan <- match.arg(plan, c("multisession", "multicore"))
  total_prompts <- length(prompts)
  
  result <- if (file.exists(state_path)) {
    existing_result <- readRDS(state_path)
    if (!identical(as.list(prompts), existing_result@prompts)) {
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
      prompts = as.list(prompts),
      responses = vector("list", total_prompts),
      completed = 0L,
      state_path = state_path,
      type_spec = type_spec,
      echo = "none",
      progress_bar = NULL,
      input_type = if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list",
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
        retry_count = 0L
      )
    )
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
  was_interrupted <- FALSE
  max_chunk_attempts <- 3
  pb <- NULL
  
  tryCatch({
    pb <- cli::cli_progress_bar(
      format = "Processing chats [{cli::pb_current}/{cli::pb_total}] [{cli::pb_bar}] {cli::pb_percent}",
      total = total_prompts
    )
    cli::cli_progress_update(id = pb, set = result@completed)
    
    for (chunk_idx in seq_along(chunks)) {
      if (was_interrupted) break
      
      chunk <- chunks[[chunk_idx]]
      chunk_attempts <- 0
      chunk_successful <- FALSE
      
      while (!chunk_successful && chunk_attempts < max_chunk_attempts) {
        chunk_attempts <- chunk_attempts + 1
        
        tryCatch({
          new_responses <- furrr::future_map(
            chunk,
            function(prompt) {
              R.utils::withTimeout({
                worker_chat <- chat_obj$clone()
                structure(
                  tryCatch({
                    response <- capture_with_retry(
                      worker_chat,
                      prompt,
                      type_spec,
                      echo = "none",
                      max_retries = 0,
                      initial_delay = 1,
                      max_delay = 16,
                      backoff_factor = 2
                    )
                    list(success = TRUE, data = response)
                  },
                  error = function(e) {
                    if (grepl("unauthorized|authentication|invalid.*key|api.*key",
                              tolower(e$message),
                              ignore.case = TRUE
                    )) {
                      list(success = FALSE, error = "auth", message = e$message)
                    } else {
                      list(success = FALSE, error = "other", message = e$message)
                    }
                  }),
                  class = "worker_response"
                )
              }, timeout = 60)
            },
            .progress = FALSE,
            .options = furrr::furrr_options(
              scheduling = 1,
              seed = TRUE
            )
          )
          
          auth_errors <- Filter(function(x) !x$success && x$error == "auth", new_responses)
          if (length(auth_errors) > 0) {
            cli::cli_alert_warning(sprintf("Interrupted at chat %d of %d", result@completed, total_prompts))
            stop(auth_errors[[1]]$message, call. = FALSE)
          }
          
          other_errors <- Filter(function(x) !x$success && x$error == "other", new_responses)
          if (length(other_errors) > 0) {
            stop(other_errors[[1]]$message, call. = FALSE)
          }
          
          successful_responses <- Filter(function(x) x$success, new_responses)
          completed_before <- result@completed
          for (i in seq_along(successful_responses)) {
            result@responses[[completed_before + i]] <- successful_responses[[i]]$data
          }
          result@completed <- completed_before + length(successful_responses)
          saveRDS(result, state_path)
          
          cli::cli_progress_update(id = pb, set = result@completed)
          chunk_successful <- TRUE
        },
        error = function(e) {
          if (grepl("unauthorized|authentication|invalid.*key|api.*key",
                    tolower(conditionMessage(e)),
                    ignore.case = TRUE
          )) {
            stop(e)
          }
          
          if (chunk_attempts >= max_chunk_attempts) {
            cli::cli_alert_danger(sprintf(
              "Chunk %d/%d failed after %d attempts: %s",
              chunk_idx, length(chunks), chunk_attempts, conditionMessage(e)
            ))
            stop(e)
          } else {
            cli::cli_alert_warning(sprintf(
              "Chunk %d/%d attempt %d failed: %s. Retrying...",
              chunk_idx, length(chunks), chunk_attempts, conditionMessage(e)
            ))
            Sys.sleep(2^chunk_attempts)
          }
        })
      }
    }
    
    if (!was_interrupted) {
      cli::cli_progress_done(id = pb)
      cli::cli_alert_success("Complete")
      if (beep) beepr::beep("ping")
    }
  },
  error = function(e) {
    if (!is.null(pb)) cli::cli_progress_done(id = pb)
    saveRDS(result, state_path)
    if (beep) beepr::beep("wilhelm")
    stop(e)
  },
  interrupt = function(e) {
    if (beep) beepr::beep("coin")
    if (!is.null(pb)) cli::cli_progress_done(id = pb)
    saveRDS(result, state_path)
    cat(sprintf("\r! Interrupted at chat %d of %d", result@completed, total_prompts))
  })
  
  create_results(result)
}

#' Process chunks of prompts in parallel
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
#' @keywords internal
update_batch_state <- function(result, new_responses, pb, state_path) {
  completed_before <- result@completed

  for (i in seq_along(new_responses)) {
    result@responses[[completed_before + i]] <- new_responses[[i]]
  }

  result@completed <- completed_before + length(new_responses)
  saveRDS(result, state_path)

  if (!is.null(pb)) {
    cli::cli_progress_update(id = pb, set = result@completed)
  }
}

#' Handle batch interruption
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
#' @keywords internal
finish_successful_batch <- function(pb, beep) {
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }
  cli::cli_alert_success("Complete")
  if (beep) beepr::beep("ping")
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
#' @param chat_model Chat model object to use for processing (default: chat_claude())
#' @param prompts List or vector of prompts to process sequentially
#' @param type_spec Type specification for structured data extraction (default: NULL)
#' @param state_path Path for saving intermediate state. Enables resuming
#'        interrupted batches from the last successful prompt. (default: tempfile("chat_batch_", fileext = ".rds"))
#' @param echo Level of output to display: "none" for silent operation,
#'        "text" for response text only, or "all" for full interaction (default: "none")
#' @param beep Logical indicating whether to play a sound when batch processing completes (default: TRUE)
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
chat_batch <- function(chat_model = chat_claude(), echo = "none", beep = TRUE, ...) {
  original_chat <- chat_model
  chat_env <- new.env(parent = emptyenv())

  assignments <- data.frame(
    names = names(original_chat),
    stringsAsFactors = FALSE
  )

  assignments$values <- lapply(assignments$names, function(n) original_chat[[n]])

  pwalk(
    assignments,
    \(names, values) assign(names, values, envir = chat_env)
  )

  chat_env$last_state_path <- NULL

  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds")) {
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }

    process(original_chat, prompts, type_spec, state_path, echo, beep)
  }

  structure(chat_env, class = class(original_chat))
}

#' Process a batch of prompts in parallel
#' @description
#' Processes a batch of chat prompts using parallel workers.
#' Splits prompts into chunks for processing while maintaining state.
#' For sequential processing, use `chat_batch()`.
#'
#' @param chat_model A chat model object (default: chat_claude())
#' @param beep Logical indicating whether to play sound on completion (default: TRUE)
#' @param workers Number of parallel workers to use (default: 4)
#' @param plan Processing strategy to use: "multisession" for separate R sessions
#'        or "multicore" for forked processes (default: "multisession")
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
chat_parallel <- function(chat_model = chat_claude(), workers = 4,
                          plan = "multisession", beep = TRUE, ...) {
  plan <- match.arg(plan, choices = c("multisession", "multicore"))
  original_chat <- chat_model
  chat_env <- new.env(parent = emptyenv())
  
  purrr::walk(names(original_chat), function(n) {
    assign(n, original_chat[[n]], envir = chat_env)
  })
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts, type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds"),
                             chunk_size = 4) {
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }
    
    process_parallel(
      original_chat, prompts, type_spec, state_path,
      chunk_size, workers, beep, plan
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
