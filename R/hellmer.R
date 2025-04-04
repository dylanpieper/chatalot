#' Process a batch of prompts in sequence
#' @description
#' Processes a batch of chat prompts one at a time in sequential order.
#' Maintains state between runs and can resume interrupted processing.
#' For parallel processing, use `chat_future()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `ellmer::chat_openai`)
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A batch object (S7 class) containing
#'   \itemize{
#'     \item prompts: Original input prompts
#'     \item responses: Raw response data for completed prompts
#'     \item completed: Number of successfully processed prompts
#'     \item state_path: Path where batch state is saved
#'     \item type_spec: Type specification used for structured data
#'     \item texts: Function to extract text responses (includes structured data when a type specification is provided)
#'     \item chats: Function to extract chat objects
#'     \item progress: Function to get processing status
#'     \item batch: Function to process a batch of prompts
#'   }
#' @section Batch Method:
#' \preformatted{
#' batch(
#'   prompts,
#'   type_spec = NULL,
#'   judgements = 0,
#'   state_path = tempfile("chat_", fileext = ".rds"),
#'   progress = TRUE,
#'   max_retries = 3L,
#'   initial_delay = 20,
#'   max_delay = 80,
#'   backoff_factor = 2,
#'   beep = TRUE,
#'   echo = FALSE,
#'   ...
#' )
#' }
#'
#' The batch method processes multiple prompts and returns a batch object:
#' \itemize{
#'   \item prompts: List of prompts to process
#'   \item type_spec: Type specification for structured data extraction
#'   \item judgements: Number of judgements for data extraction accuracy
#'   \item state_path: Path to save state file for resuming interrupted processing
#'   \item progress: Whether to show progress bars (default: TRUE)
#'   \item max_retries: Maximum number of retry attempts for failed requests
#'   \item initial_delay: Initial delay before first retry in seconds
#'   \item max_delay: Maximum delay between retries in seconds
#'   \item backoff_factor: Factor to multiply delay by after each retry
#'   \item beep: Whether to play a sound on completion
#'   \item echo: Whether to display chat outputs (when progress is FALSE)
#'   \item ...: Additional arguments passed to the chat method
#' }
#'
#' @examplesIf ellmer::has_credentials("openai")
#' # Create a sequential chat processor with an object
#' chat <- chat_sequential(chat_openai(system_prompt = "Reply concisely"))
#'
#' # Or a function
#' chat <- chat_sequential(chat_openai, system_prompt = "Reply concisely, one sentence")
#'
#' # Process a batch of prompts in sequence
#' batch <- chat$batch(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse",
#'     "Explain vectors, lists, and data frames"
#'   ),
#'   max_retries = 3L,
#'   initial_delay = 20,
#'   beep = TRUE
#' )
#'
#' # Process batch with echo enabled (when progress is disabled)
#' batch <- chat$batch(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse"
#'   ),
#'   progress = FALSE,
#'   echo = TRUE
#' )
#'
#' # Check the progress if interrupted
#' batch$progress()
#'
#' # Return the responses
#' batch$texts()
#'
#' # Return the chat objects
#' batch$chats()
#' @export
chat_sequential <- function(
    chat_model = NULL,
    ...) {
  if (is.null(chat_model)) {
    stop("Define an ellmer chat model (e.g., chat_openai)")
  }
  
  chat_env <- new.env(parent = emptyenv())
  
  if (is.function(chat_model)) {
    chat_env$chat_model <- chat_model(...)
  } else {
    chat_env$chat_model <- chat_model
  }
  
  for (n in names(chat_env$chat_model)) {
    chat_env[[n]] <- chat_env$chat_model[[n]]
  }
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             judgements = 0,
                             state_path = tempfile("chat_", fileext = ".rds"),
                             progress = TRUE,
                             max_retries = 3L,
                             initial_delay = 20,
                             max_delay = 80,
                             backoff_factor = 2,
                             beep = TRUE,
                             echo = FALSE,
                             ...) {
    if (judgements > 0 && is.null(type_spec)) {
      cli::cli_alert_warning("Judgements parameter ({judgements}) specified but will be ignored without a type_spec")
    }
    
    if (!is.null(type_spec) && judgements < 0) {
      cli::cli_abort("Number of judgements must be non-negative")
    }
    
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }
    
    process(
      chat_obj = chat_env$chat_model,
      prompts = prompts,
      type_spec = type_spec,
      judgements = judgements,
      state_path = state_path,
      progress = progress,
      max_retries = max_retries,
      initial_delay = initial_delay,
      max_delay = max_delay,
      backoff_factor = backoff_factor,
      beep = beep,
      echo = echo,
      ...
    )
  }
  
  class(chat_env) <- c("Chat", "R6")
  chat_env
}

#' Process a batch of prompts in parallel
#' @description
#' Processes a batch of chat prompts using parallel workers.
#' Splits prompts into chunks for processing while maintaining state.
#' For sequential processing, use `chat_sequential()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `ellmer::chat_openai`)
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A batch object (S7 class) containing:
#'   \itemize{
#'     \item prompts: Original input prompts
#'     \item responses: Raw response data for completed prompts
#'     \item completed: Number of successfully processed prompts
#'     \item state_path: Path where batch state is saved
#'     \item type_spec: Type specification used for structured data
#'     \item texts: Function to extract text responses (includes structured data when a type specification is provided)
#'     \item chats: Function to extract chat objects
#'     \item progress: Function to get processing status
#'     \item batch: Function to process a batch of prompts
#'   }
#' @section Batch Method:
#' \preformatted{
#' batch(
#'   prompts,
#'   type_spec = NULL,
#'   judgements = 0,
#'   state_path = tempfile("chat_", fileext = ".rds"),
#'   progress = TRUE,
#'   workers = parallel::detectCores(),
#'   plan = "multisession",
#'   chunk_size = NULL,
#'   max_chunk_attempts = 3L,
#'   max_retries = 3L,
#'   initial_delay = 20,
#'   max_delay = 80,
#'   backoff_factor = 2,
#'   beep = TRUE,
#'   echo = FALSE,
#'   ...
#' )
#' }
#'
#' The batch method processes multiple prompts in parallel and returns a batch object:
#' \itemize{
#'   \item prompts: List of prompts to process
#'   \item type_spec: Type specification for structured data extraction
#'   \item judgements: Number of judgements for data extraction accuracy
#'   \item state_path: Path to save state file for resuming interrupted processing
#'   \item progress: Whether to show progress bars (default: TRUE)
#'   \item workers: Number of parallel workers
#'   \item plan: Parallel backend plan ("multisession" or "multicore")
#'   \item chunk_size: Size of chunks for parallel processing
#'   \item max_chunk_attempts: Maximum retries per failed chunk
#'   \item max_retries: Maximum number of retry attempts for failed requests
#'   \item initial_delay: Initial delay before first retry in seconds
#'   \item max_delay: Maximum delay between retries in seconds
#'   \item backoff_factor: Factor to multiply delay by after each retry
#'   \item beep: Whether to play a sound on completion
#'   \item echo: Whether to display chat outputs (when progress=FALSE)
#'   \item ...: Additional arguments passed to the chat method
#' }
#'
#' @examplesIf ellmer::has_credentials("openai")
#' # Create a parallel chat processor with an object
#' chat <- chat_future(chat_openai(system_prompt = "Reply concisely"))
#'
#' # Or a function
#' chat <- chat_future(chat_openai, system_prompt = "Reply concisely, one sentence")
#'
#' # Process a batch of prompts in parallel
#' batch <- chat$batch(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse",
#'     "Explain vectors, lists, and data frames"
#'   ),
#'   chunk_size = 3
#' )
#'
#' # Process batch with echo enabled (when progress is disabled)
#' batch <- chat$batch(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse"
#'   ),
#'   progress = FALSE, 
#'   echo = TRUE
#' )
#'
#' # Check the progress if interrupted
#' batch$progress()
#'
#' # Return the responses
#' batch$texts()
#'
#' # Return the chat objects
#' batch$chats()
#' @export
chat_future <- function(
    chat_model = NULL,
    ...) {
  if (is.null(chat_model)) {
    stop("Define an ellmer chat_model (e.g., chat_openai)")
  }
  
  chat_env <- new.env(parent = emptyenv())
  
  if (is.function(chat_model)) {
    chat_env$chat_model <- chat_model(...)
  } else {
    chat_env$chat_model <- chat_model
  }
  
  for (n in names(chat_env$chat_model)) {
    chat_env[[n]] <- chat_env$chat_model[[n]]
  }
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             judgements = 0,
                             state_path = tempfile("chat_", fileext = ".rds"),
                             progress = TRUE,
                             workers = parallel::detectCores(),
                             plan = "multisession", 
                             chunk_size = NULL,
                             max_chunk_attempts = 3L,
                             max_retries = 3L,
                             initial_delay = 20,
                             max_delay = 80,
                             backoff_factor = 2,
                             beep = TRUE,
                             echo = FALSE,
                             ...) {
    
    plan <- match.arg(plan, choices = c("multisession", "multicore"))
    
    if (judgements > 0 && is.null(type_spec)) {
      cli::cli_alert_warning("Judgements parameter ({judgements}) specified but will be ignored without a type_spec")
    }
    
    if (!is.null(type_spec) && judgements < 0) {
      cli::cli_abort("Number of judgements must be non-negative")
    }
    
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }
    
    process_future(
      chat_obj = chat_env$chat_model,
      prompts = prompts,
      type_spec = type_spec,
      judgements = judgements,
      state_path = state_path,
      workers = workers,
      chunk_size = chunk_size,
      plan = plan,
      max_chunk_attempts = max_chunk_attempts,
      max_retries = max_retries,
      initial_delay = initial_delay,
      max_delay = max_delay,
      backoff_factor = backoff_factor,
      beep = beep,
      progress = progress,
      echo = echo,
      ...
    )
  }
  
  class(chat_env) <- c("Chat", "R6")
  chat_env
}