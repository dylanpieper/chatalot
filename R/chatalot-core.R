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
  chat_response <- NULL
  structured_data <- NULL
  chat <- original_chat$clone()

  args <- as.list(prompt)

  chats_obj <- withCallingHandlers(
    {
      if (!is.null(type)) {
        structured_data <- do.call(
          chat$chat_structured,
          c(args, list(type = type), list(...))
        )

        if (is.null(structured_data)) {
          stop("Received NULL structured data extraction")
        }
      } else {
        chat_response <- do.call(
          chat$chat,
          c(args, list(echo = echo), list(...))
        )

        if (is.null(chat_response)) {
          stop("Received NULL chat response")
        }
      }

      list(
        chat = chat,
        text = chat_response,
        structured_data = structured_data
      )
    },
    interrupt = function(e) {
      signalCondition(e)
    }
  )

  return(chats_obj)
}

#' Match new prompts against old prompts and preserve completed responses
#' @param new_prompts New list of prompts
#' @param old_prompts Old list of prompts from saved state
#' @param old_responses Old responses from saved state
#' @return List containing responses, completed count, and restart flag
#' @keywords internal
#' @noRd
match_prompts <- function(new_prompts, old_prompts, old_responses) {
  n_new <- length(new_prompts)
  n_old <- length(old_prompts)
  min_len <- min(n_new, n_old)

  matches <- if (min_len > 0) {
    vapply(seq_len(min_len), function(i) identical(new_prompts[[i]], old_prompts[[i]]), logical(1))
  } else {
    logical(0)
  }

  new_responses <- vector("list", n_new)
  if (any(matches)) {
    match_indices <- which(matches)
    new_responses[match_indices] <- old_responses[match_indices]
  }

  n_completed <- sum(matches & !vapply(old_responses[seq_len(min_len)], is.null, logical(1)))

  list(
    responses = new_responses,
    completed = n_completed,
    should_restart = n_completed == 0 && !any(matches)
  )
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
    chats_obj <- readRDS(file)
    result <- match_prompts(as.list(prompts), chats_obj@prompts, chats_obj@responses)

    if (result$should_restart) {
      cli::cli_alert_warning("No matching prompts found, starting fresh")
      unlink(file)
      chats_obj <- NULL
    } else {
      cli::cli_alert_info("Preserving {result$completed} completed response{?s}")
      chats_obj@prompts <- as.list(prompts)
      chats_obj@responses <- result$responses
      chats_obj@completed <- result$completed
      chats_obj@chat_status <- ifelse(vapply(result$responses, is.null, logical(1)), "pending", "completed")
    }
  } else {
    chats_obj <- NULL
  }

  if (is.null(chats_obj)) {
    chats_obj <- process(
      prompts = as.list(prompts),
      responses = vector("list", length(prompts)),
      completed = 0L,
      file = file,
      type = type,
      progress = progress,
      input_type = if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list",
      workers = NULL
    )
    saveRDS(chats_obj, file)
  }

  total_prompts <- length(prompts)

  if (chats_obj@completed >= total_prompts) {
    if (progress) {
      cli::cli_alert_success("Complete")
    }
    return(finish_chats_obj(chats_obj))
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
    cli::cli_progress_update(id = pb, set = chats_obj@completed)
  }

  tryCatch({
    for (i in (chats_obj@completed + 1L):total_prompts) {
      response <- capture(
        chat_obj, prompts[[i]], type,
        echo = echo,
        ...
      )

      chats_obj@responses[[i]] <- response
      chats_obj@completed <- i
      saveRDS(chats_obj, file)

      if (!is.null(pb)) {
        cli::cli_progress_update(id = pb, set = i)
      }
    }

    finish_process(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(chats_obj, file)

    if (inherits(e, "interrupt")) {
      handle_interrupt(chats_obj, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }

    saveRDS(chats_obj, file)

    if (beep) beepr::beep("coin")
    cli::cli_alert_warning(sprintf(
      "Interrupted at chat %d of %d",
      chats_obj@completed, total_prompts
    ))
  }, finally = {
    if (!exists("chats_obj")) {
      chats_obj <- readRDS(file)
    }
  })

  finish_chats_obj(chats_obj)
}

#' Synchronize completed integer with chat_status vector
#' @param process_obj Process object to synchronize
#' @return Updated process object with synchronized fields
#' @keywords internal
#' @noRd
sync_status_fields <- function(process_obj) {
  n_prompts <- length(process_obj@prompts)

  if (length(process_obj@chat_status) == 0) {
    process_obj@chat_status <- rep("pending", n_prompts)
    if (process_obj@completed > 0) {
      completed_indices <- seq_len(min(process_obj@completed, n_prompts))
      process_obj@chat_status[completed_indices] <- "completed"
    }
  }
  process_obj@completed <- sum(process_obj@chat_status == "completed", na.rm = TRUE)

  process_obj
}

#' Check if there are remaining tasks
#' @param process_obj Process object to check
#' @return TRUE if there are pending or failed chats, FALSE otherwise
#' @keywords internal
#' @noRd
has_remaining_work <- function(process_obj) {
  any(process_obj@chat_status %in% c("pending", "failed"))
}

#' Process prompts in parallel chunks with error handling and state management
#' @param chat_obj Chat model object for API calls
#' @param prompts Vector or list of prompts to process
#' @param type Optional type specification for structured data extraction
#' @param file Path to save intermediate state
#' @param workers Number of parallel workers
#' @param beep Play sound on completion/error
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
    beep,
    progress,
    echo,
    ...) {
  validate_chunk <- function(chunk_chats_obj, chunk_idx) {
    if (inherits(chunk_chats_obj, "error") || inherits(chunk_chats_obj, "worker_error")) {
      return(list(valid = FALSE, message = conditionMessage(chunk_chats_obj)))
    }

    if (!is.list(chunk_chats_obj) || !("responses" %in% names(chunk_chats_obj))) {
      return(list(valid = FALSE, message = sprintf("Invalid chunk structure in chunk %d", chunk_idx)))
    }

    if (length(chunk_chats_obj$responses) == 0) {
      return(list(valid = FALSE, message = sprintf("Empty responses in chunk %d", chunk_idx)))
    }

    null_indices <- which(vapply(chunk_chats_obj$responses, is.null, logical(1)))
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
    chats_obj <- readRDS(file)
    result <- match_prompts(prompts_list, chats_obj@prompts, chats_obj@responses)

    if (result$should_restart) {
      cli::cli_alert_warning("No matching prompts found, starting fresh")
      unlink(file)
      chats_obj <- NULL
    } else {
      cli::cli_alert_info("Preserving {result$completed} completed response{?s}")
      chats_obj@prompts <- prompts_list
      chats_obj@responses <- result$responses
      chats_obj@completed <- result$completed
      chats_obj@chat_status <- ifelse(vapply(result$responses, is.null, logical(1)), "pending", "completed")
    }
  } else {
    chats_obj <- NULL
  }

  if (is.null(chats_obj)) {
    chats_obj <- process(
      prompts = prompts_list,
      responses = vector("list", total_prompts),
      completed = 0L,
      file = file,
      type = type,
      progress = progress,
      input_type = original_type,
      workers = as.integer(workers),
      chat_status = rep("pending", total_prompts)
    )
    saveRDS(chats_obj, file)
  }

  if (chats_obj@completed >= total_prompts) {
    if (progress) {
      cli::cli_alert_success("Complete")
    }
    return(finish_chats_obj(chats_obj))
  }

  chats_obj <- sync_status_fields(chats_obj)

  future::plan(future::multisession, workers = workers)

  pb <- NULL
  if (progress) {
    pb <- cli::cli_progress_bar(
      format = "Processing chats [{cli::pb_current}/{cli::pb_total}] [{cli::pb_bar}] {cli::pb_eta}",
      total = total_prompts
    )
    cli::cli_progress_update(id = pb, set = chats_obj@completed)
  }

  capture_future <- capture

  tryCatch({
    while (has_remaining_work(chats_obj)) {
      remaining_indices <- which(chats_obj@chat_status %in% c("pending", "failed"))
      current_batch <- head(remaining_indices, workers)

      tool_globals <- list()
      if (is.environment(chat_obj) && exists("deferred_tools", envir = chat_obj) && length(chat_obj$deferred_tools) > 0) {
        for (tool_with_data in chat_obj$deferred_tools) {
          if ("globals" %in% names(tool_with_data) && length(tool_with_data$globals) > 0) {
            tool_globals <- c(tool_globals, tool_with_data$globals)
          }
        }
      }

      batch_results <- furrr::future_map(current_batch, function(i) {
        if (is.environment(chat_obj) && exists("chat_model_name", envir = chat_obj)) {
          worker_chat <- if (is.character(chat_obj$chat_model_name)) {
            constructed_chat <- do.call(ellmer::chat, c(list(chat_obj$chat_model_name), chat_obj$chat_model_args))

            if (exists("deferred_tools", envir = chat_obj) && length(chat_obj$deferred_tools) > 0) {
              for (tool_with_data in chat_obj$deferred_tools) {
                tool <- tool_with_data$tool
                constructed_chat$register_tool(tool)
              }
            }

            constructed_chat
          } else {
            stop("Invalid deferred chat construction")
          }
        } else {
          worker_chat <- chat_obj$clone()
        }

        capture_future(worker_chat, chats_obj@prompts[[i]], type, echo = echo, ...)
      }, .options = furrr::furrr_options(
        scheduling = 1,
        seed = TRUE,
        globals = c(list(chat_obj = chat_obj, type = type, echo = echo, capture_future = capture_future), tool_globals)
      ))

      for (j in seq_along(current_batch)) {
        idx <- current_batch[j]
        if (!inherits(batch_results[[j]], "error")) {
          chats_obj@responses[[idx]] <- batch_results[[j]]
          chats_obj@chat_status[idx] <- "completed"
          chats_obj@completed <- sum(chats_obj@chat_status == "completed")

          saveRDS(chats_obj, file)

          if (!is.null(pb)) {
            cli::cli_progress_update(id = pb, set = chats_obj@completed)
          }
        } else {
          chats_obj@chat_status[idx] <- "failed"
          saveRDS(chats_obj, file)
        }
      }
    }

    finish_process(pb, beep, progress)
  }, error = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(chats_obj, file)

    if (inherits(e, "interrupt")) {
      handle_interrupt(chats_obj, beep)
    } else {
      if (beep) beepr::beep("wilhelm")
      stop(e)
    }
  }, interrupt = function(e) {
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb)
    }
    saveRDS(chats_obj, file)

    if (beep) beepr::beep("coin")
    cli::cli_alert_warning(sprintf(
      "Interrupted at chat %d of %d",
      chats_obj@completed, total_prompts
    ))
  }, finally = {
    if (!exists("chats_obj")) {
      chats_obj <- readRDS(file)
    }
    future::plan(future::sequential)
  })

  finish_chats_obj(chats_obj)
}

#' Process chunks of prompts in parallel
#' @param chunks List of prompt chunks to process
#' @param chats_obj A process object to store results
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
                           chats_obj,
                           chat_obj,
                           type,
                           pb,
                           file,
                           progress,
                           beep,
                           echo,
                           ...) {
  interrupted <- FALSE
  capture_future <- capture

  for (chunk in chunks) {
    if (interrupted) break

    withCallingHandlers(
      {
        new_responses <- furrr::future_map(
          chunk,
          function(prompt) {
            if (is.environment(chat_obj) && exists("chat_model_name", envir = chat_obj)) {
              worker_chat <- if (is.character(chat_obj$chat_model_name)) {
                do.call(ellmer::chat, c(list(chat_obj$chat_model_name), chat_obj$chat_model_args))
              } else {
                stop("Invalid deferred chat construction")
              }
            } else {
              worker_chat <- chat_obj$clone()
            }

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

        start_idx <- chats_obj@completed + 1
        end_idx <- chats_obj@completed + length(new_responses)
        chats_obj@responses[start_idx:end_idx] <- new_responses
        chats_obj@completed <- end_idx
        saveRDS(chats_obj, file)
        if (!is.null(pb)) {
          cli::cli_progress_update(id = pb, set = end_idx)
        }
      },
      interrupt = function(e) {
        interrupted <<- TRUE
        handle_interrupt(chats_obj, beep)
        invokeRestart("abort")
      }
    )
  }

  if (!interrupted) {
    finish_process(pb, beep, progress)
  }
}


#' Handle interruption
#' @name handle_interrupt
#' @usage handle_interrupt(chats_obj, beep)
#' @param chats_obj A process object containing processing state
#' @param beep Logical indicating whether to play a sound
#' @return NULL (called for side effects)
#' @keywords internal
#' @noRd
handle_interrupt <- function(chats_obj, beep) {
  cli::cli_alert_warning(
    sprintf(
      "Interrupted at chat %d of %d",
      chats_obj@completed, length(chats_obj@prompts)
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

#' Finish chats object by converting it to a list and assigning functions
#' @param chats_obj Process object
#' @return List with class "process"
#' @keywords internal
#' @noRd
finish_chats_obj <- function(chats_obj) {
  chats_list <- list(
    prompts = chats_obj@prompts,
    responses = chats_obj@responses,
    completed = chats_obj@completed,
    file = chats_obj@file,
    type = chats_obj@type
  )

  chats_list$texts <- function() texts(chats_obj)
  chats_list$chats <- function() chats(chats_obj)
  chats_list$progress <- function() progress(chats_obj)

  structure(chats_list, class = "process")
}
