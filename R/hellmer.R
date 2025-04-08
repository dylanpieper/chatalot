#' Process a batch of prompts in sequence
#' @description
#' Processes a batch of chat prompts one at a time in sequential order.
#' Maintains state between runs and can resume interrupted processing.
#' For parallel processing, use `chat_future()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `chat_openai()`)
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A batch object (S7 class) containing
#'   \itemize{
#'     \item **prompts**: Original input prompts
#'     \item **responses**: Raw response data for completed prompts
#'     \item **completed**: Number of successfully processed prompts
#'     \item **state_path**: Path where batch state is saved
#'     \item **type_spec**: Type specification used for structured data
#'     \item **texts**: Function to extract text responses or structured data
#'     \item **chats**: Function to extract chat objects
#'     \item **progress**: Function to get processing status
#'     \item **batch**: Function to process a batch of prompts
#'   }
#' @section Batch Method:
#' This function provides access to the `batch()` method for sequential processing of prompts.
#' See `?batch.sequential_chat` for full details of the method and its parameters.
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
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }

    batch.sequential_chat(
      chat_env = chat_env,
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

  class(chat_env) <- c("sequential_chat", "Chat", "R6")
  chat_env
}

#' Process a batch of prompts in parallel
#' @description
#' Processes a batch of chat prompts using parallel workers.
#' Splits prompts into chunks for processing while maintaining state.
#' For sequential processing, use `chat_sequential()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `chat_openai()`)
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A batch object (S7 class) containing:
#'   \itemize{
#'     \item **prompts**: Original input prompts
#'     \item **responses**: Raw response data for completed prompts
#'     \item **completed**: Number of successfully processed prompts
#'     \item **state_path**: Path where batch state is saved
#'     \item **type_spec**: Type specification used for structured data
#'     \item **texts**: Function to extract text responses or structured data
#'     \item **chats**: Function to extract chat objects
#'     \item **progress**: Function to get processing status
#'     \item **batch**: Function to process a batch of prompts
#'   }
#' @section Batch Method:
#' This function provides access to the `batch()` method for parallel processing of prompts.
#' See `?batch.future_chat` for full details of the method and its parameters.
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
    cli::cli_abort("Define an ellmer chat_model (e.g., chat_openai)")
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
                             workers = NULL,
                             plan = "multisession",
                             chunk_size = parallel::detectCores() * 5,
                             max_chunk_attempts = 3L,
                             max_retries = 3L,
                             initial_delay = 20,
                             max_delay = 80,
                             backoff_factor = 2,
                             beep = TRUE,
                             echo = FALSE,
                             ...) {
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }

    if (is.null(workers)) {
      if (length(prompts) <= parallel::detectCores()) {
        workers <- length(prompts)
      } else {
        workers <- parallel::detectCores()
      }
    }

    batch.future_chat(
      chat_env = chat_env,
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

  class(chat_env) <- c("future_chat", "Chat", "R6")
  chat_env
}
