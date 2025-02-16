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
    timeout = 60,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
    ...
) {
  chat_env <- new.env(parent = emptyenv())
  chat_env$chat_model <- chat_model(echo = "none", ...)
  chat_env$echo <- echo
  chat_env$beep <- beep
  chat_env$timeout <- timeout
  chat_env$max_retries <- max_retries
  chat_env$initial_delay <- initial_delay
  chat_env$max_delay <- max_delay
  chat_env$backoff_factor <- backoff_factor
  chat_env$last_state_path <- NULL
  chat_env$register_tool <- chat_env$chat_model$register_tool
  chat_env$chat <- chat_env$chat_model$chat
  chat_env$extract_data <- chat_env$chat_model$extract_data
  chat_env$get_turns <- chat_env$chat_model$get_turns
  chat_env$tokens <- chat_env$chat_model$tokens
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds")) {
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }
    
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
#' @param max_retries Maximum number of retry attempts per prompt (default: 3)
#' @param initial_delay Initial delay in seconds before first retry (default: 1)
#' @param max_delay Maximum delay in seconds between retries (default: 32)
#' @param backoff_factor Factor to multiply delay by after each retry (default: 2)
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
    chat_model = ellmer::chat_claude(),
    workers = 4,
    plan = "multisession",
    beep = TRUE,
    timeout = 60,
    max_chunk_attempts = 3L,
    max_retries = 3L,
    initial_delay = 1,
    max_delay = 32,
    backoff_factor = 2,
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
      max_chunk_attempts = max_chunk_attempts,
      max_retries = max_retries,
      initial_delay = initial_delay,
      max_delay = max_delay,
      backoff_factor = backoff_factor
    )
  }
  
  structure(chat_env, class = class(original_chat))
}