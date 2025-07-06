#' Process a lot of prompts in sequence
#' @description
#' Processes a lot of chat prompts one at a time in sequential order.
#' Maintains state between runs and can resume interrupted processing.
#' For parallel processing, use `chat_future()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `chat_openai()`)
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A lot object (S7 class) containing
#'   \itemize{
#'     \item **prompts**: Original input prompts
#'     \item **responses**: Raw response data for completed prompts
#'     \item **completed**: Number of successfully processed prompts
#'     \item **file**: Path where batch state is saved
#'     \item **type**: Type specification used for structured data
#'     \item **texts**: Function to extract text responses or structured data
#'     \item **chats**: Function to extract chat objects
#'     \item **progress**: Function to get processing status
#'     \item **lot**: Function to process a lot of prompts
#'   }
#' @section Lot Method:
#' This function provides access to the `lot()` method for sequential processing of prompts.
#' See `?lot.sequential_chat` for full details of the method and its parameters.
#'
#' @examplesIf ellmer::has_credentials("openai")
#' # Create a sequential chat processor with an object
#' chat <- chat_sequential(chat_openai(system_prompt = "Reply concisely"))
#'
#' # Or a function
#' chat <- chat_sequential(chat_openai, system_prompt = "Reply concisely, one sentence")
#'
#' # Process a lot of prompts in sequence
#' lot <- chat$lot(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse",
#'     "Explain vectors, lists, and data frames"
#'   ),
#'   beep = TRUE
#' )
#'
#' # Process batch with echo enabled (when progress is disabled)
#' lot <- chat$lot(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse"
#'   ),
#'   progress = FALSE,
#'   echo = TRUE
#' )
#'
#' # Check the progress if interrupted
#' lot$progress()
#'
#' # Return the responses
#' lot$texts()
#'
#' # Return the chat objects
#' lot$chats()
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

  chat_env$last_file <- NULL

  chat_env$lot <- function(prompts,
                             type = NULL,
                             eval = FALSE,
                             file = tempfile("chat_", fileext = ".rds"),
                             progress = TRUE,
                             beep = TRUE,
                             echo = FALSE,
                             ...) {
    if (is.null(chat_env$last_file)) {
      chat_env$last_file <- file
    } else {
      file <- chat_env$last_file
    }

    lot.sequential_chat(
      chat_env = chat_env,
      prompts = prompts,
      type = type,
      eval = eval,
      file = file,
      progress = progress,
      beep = beep,
      echo = echo,
      ...
    )
  }

  class(chat_env) <- c("sequential_chat", "Chat", "R6")
  chat_env
}

#' Process a lot of prompts in parallel
#' @description
#' Processes a lot of chat prompts using parallel workers.
#' Splits prompts into chunks for processing while maintaining state.
#' For sequential processing, use `chat_sequential()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `chat_openai()`)
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A lot object (S7 class) containing:
#'   \itemize{
#'     \item **prompts**: Original input prompts
#'     \item **responses**: Raw response data for completed prompts
#'     \item **completed**: Number of successfully processed prompts
#'     \item **file**: Path where batch state is saved
#'     \item **type**: Type specification used for structured data
#'     \item **texts**: Function to extract text responses or structured data
#'     \item **chats**: Function to extract chat objects
#'     \item **progress**: Function to get processing status
#'     \item **lot**: Function to process a lot of prompts
#'   }
#' @section Lot Method:
#' This function provides access to the `lot()` method for parallel processing of prompts.
#' See `?lot.future_chat` for full details of the method and its parameters.
#'
#' @examplesIf interactive() && ellmer::has_credentials("openai")
#' # Create a parallel chat processor with an object
#' chat <- chat_future(chat_openai(system_prompt = "Reply concisely"))
#'
#' # Or a function
#' chat <- chat_future(chat_openai, system_prompt = "Reply concisely, one sentence")
#'
#' # Process a lot of prompts in parallel
#' lot <- chat$lot(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse",
#'     "Explain vectors, lists, and data frames"
#'   ),
#'   chunk_size = 3
#' )
#'
#' # Process batch with echo enabled (when progress is disabled)
#' lot <- chat$lot(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse"
#'   ),
#'   progress = FALSE,
#'   echo = TRUE
#' )
#'
#' # Check the progress if interrupted
#' lot$progress()
#'
#' # Return the responses
#' lot$texts()
#'
#' # Return the chat objects
#' lot$chats()
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

  chat_env$last_file <- NULL

  chat_env$lot <- function(prompts,
                             type = NULL,
                             eval = FALSE,
                             file = tempfile("chat_", fileext = ".rds"),
                             progress = TRUE,
                             workers = NULL,
                             chunk_size = parallel::detectCores(),
                             max_chunk_attempts = 3L,
                             beep = TRUE,
                             echo = FALSE,
                             ...) {
    if (is.null(chat_env$last_file)) {
      chat_env$last_file <- file
    } else {
      file <- chat_env$last_file
    }

    if (is.null(workers)) {
      if (length(prompts) <= parallel::detectCores()) {
        workers <- length(prompts)
      } else {
        workers <- parallel::detectCores()
      }
    }

    lot.future_chat(
      chat_env = chat_env,
      prompts = prompts,
      type = type,
      eval = eval,
      file = file,
      workers = workers,
      chunk_size = chunk_size,
      max_chunk_attempts = max_chunk_attempts,
      beep = beep,
      progress = progress,
      echo = echo,
      ...
    )
  }

  class(chat_env) <- c("future_chat", "Chat", "R6")
  chat_env
}
