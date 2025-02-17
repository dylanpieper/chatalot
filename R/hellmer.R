#' Process a batch of prompts in sequence
#' @description
#' Processes a batch of chat prompts one at a time in sequential order.
#' Maintains state between runs and can resume interrupted processing.
#' For parallel processing, use `chat_parallel()`.
#'
#' @param chat_model Chat model function/object (default: `ellmer::chat_claude`)
#' @param echo Level of output to display: "none" for silent operation,
#'        "text" for response text only, or "all" for full interaction (default: "none")
#' @param beep Logical to play a sound on batch completion, interruption, and error (default: TRUE)
#' @param max_retries Maximum number of retry attempts per prompt (default: 3)
#' @param initial_delay Initial delay in seconds before first retry (default: 1)
#' @param max_delay Maximum delay in seconds between retries (default: 32)
#' @param backoff_factor Factor to multiply delay by after each retry (default: 2)
#' @param timeout Maximum time in seconds to wait for each prompt response (default: 60)
#' @param ... Additional arguments passed to the underlying chat model
#' @return A batch results object containing:
#'   \itemize{
#'     \item prompts: Original input prompts
#'     \item responses: Raw response data for completed prompts
#'     \item completed: Number of successfully processed prompts
#'     \item state_path: Path where batch state is saved
#'     \item type_spec: Type specification used for structured data
#'     \item texts: Function to extract text responses
#'     \item chats: Function to extract chat objects
#'     \item progress: Function to get processing status
#'     \item structured_data: Function to extract structured data (if `type_spec` was provided)
#'   }
#' @export
chat_batch <- function(
    chat_model = ellmer::chat_claude,
    echo = "none",
    beep = TRUE,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
    timeout = 60,
    ...
) {
  chat_env <- new.env(parent = emptyenv())
  chat_env$chat_model <- if (is.function(chat_model)) {
    chat_model(echo = "none", ...)
  } else {
    chat_model
  }
  
  purrr::map(names(chat_env$chat_model), ~{
    chat_env[[.x]] <- chat_env$chat_model[[.x]]
  })
  
  params <- list(
    echo = echo,
    beep = beep,
    timeout = timeout,
    max_retries = max_retries,
    initial_delay = initial_delay,
    max_delay = max_delay,
    backoff_factor = backoff_factor
  )
  
  purrr::iwalk(params, ~{
    chat_env[[.y]] <- .x
  })
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds")) {
    chat_env$last_state_path <- if (is.null(chat_env$last_state_path)) {
      state_path
    } else {
      chat_env$last_state_path
    }
    
    process(
      chat_obj = chat_env$chat_model,
      prompts = prompts,
      type_spec = type_spec,
      state_path = chat_env$last_state_path,
      echo = chat_env$echo,
      beep = chat_env$beep,
      max_retries = chat_env$max_retries,
      initial_delay = chat_env$initial_delay,
      max_delay = chat_env$max_delay,
      backoff_factor = chat_env$backoff_factor,
      timeout = chat_env$timeout
    )
  }
  
  class(chat_env) <- c("Chat", "R6")
  chat_env
}

#' Process a batch of prompts in parallel
#' @description
#' Processes a batch of chat prompts using parallel workers.
#' Splits prompts into chunks for processing while maintaining state.
#' For sequential processing, use `chat_batch()`.
#'
#' @param chat_model Chat model function/object (default: `ellmer::chat_claude`)
#' @param workers Number of parallel workers to use (default: 4)
#' @param plan Processing strategy to use: "multisession" for separate R sessions
#'        or "multicore" for forked processes (default: "multisession")
#' @param beep Logical to play a sound on batch completion, interruption, and error (default: TRUE)
#' @param chunk_size Number of prompts to process in each parallel chunk (default: 4)
#' @param max_chunk_attempts Maximum number of retry attempts for failed chunks (default: 3)
#' @param max_retries Maximum number of retry attempts per prompt (default: 3)
#' @param initial_delay Initial delay in seconds before first retry (default: 1)
#' @param max_delay Maximum delay in seconds between retries (default: 32)
#' @param backoff_factor Factor to multiply delay by after each retry (default: 2)
#' @param timeout Maximum time in seconds to wait for each prompt response (default: 2)
#' @param ... Additional arguments passed to the chat model
#' @return A batch results object containing:
#'   \itemize{
#'     \item prompts: Original input prompts
#'     \item responses: Raw response data for completed prompts
#'     \item completed: Number of successfully processed prompts
#'     \item state_path: Path where batch state is saved
#'     \item type_spec: Type specification used for structured data
#'     \item texts: Function to extract text responses
#'     \item chats: Function to extract chat objects
#'     \item progress: Function to get processing status
#'     \item structured_data: Function to extract structured data (if `type_spec` was provided)
#'   }
#' @export
chat_parallel <- function(
    chat_model = ellmer::chat_claude,
    workers = 4,
    plan = "multisession",
    beep = TRUE,
    chunk_size = 4L,
    max_chunk_attempts = 3L,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
    timeout = 60,
    ...) {
  
  plan <- match.arg(plan, choices = c("multisession", "multicore"))
  chat_env <- new.env(parent = emptyenv())
  chat_env$chat_model <- if (is.function(chat_model)) {
    chat_model(echo = "none", ...)
  } else {
    chat_model
  }
  
  purrr::map(names(chat_env$chat_model), ~{
    chat_env[[.x]] <- chat_env$chat_model[[.x]]
  })
  
  params <- list(
    workers = workers,
    plan = plan,
    beep = beep,
    timeout = timeout,
    max_chunk_attempts = max_chunk_attempts,
    max_retries = max_retries,
    initial_delay = initial_delay,
    max_delay = max_delay,
    backoff_factor = backoff_factor,
    chunk_size = chunk_size
  )

  purrr::iwalk(params, ~{
    chat_env[[.y]] <- .x
  })
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds"),
                             chunk_size = chat_env$chunk_size) {
    chat_env$last_state_path <- if (is.null(chat_env$last_state_path)) {
      state_path
    } else {
      chat_env$last_state_path
    }
    
    process_parallel(
      chat_obj = chat_env$chat_model,
      prompts = prompts,
      type_spec = type_spec,
      state_path = chat_env$last_state_path,
      workers = chat_env$workers,
      plan = chat_env$plan,
      beep = chat_env$beep,
      chunk_size = chunk_size,
      max_chunk_attempts = chat_env$max_chunk_attempts,
      max_retries = chat_env$max_retries,
      initial_delay = chat_env$initial_delay,
      max_delay = chat_env$max_delay,
      backoff_factor = chat_env$backoff_factor,
      timeout = chat_env$timeout
    )
  }
  
  class(chat_env) <- c("Chat", "R6")
  chat_env
}