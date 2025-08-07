#' Process a lot of prompts in sequence
#' @description
#' Process a lot of chat prompts in sequence, or one at a time.
#' For persistent caching, save each chat to the disk and
#' resume processing from the last saved chat.
#' Use this function to process prompts slowly, such as when you
#' have strict rate limits or want to periodically check the responses.
#' which processes one request at a time anyway.
#' For parallel processing, use `future_chat()`.
#'
#' @param chat_model Character string specifying the chat model to use (e.g., "openai/gpt-4.1" or "anthropic/claude-3-5-sonnet-latest").
#'   This creates an ellmer chat object using [ellmer::chat()].
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A process object (S7 class) containing
#'   \itemize{
#'     \item **prompts**: Original input prompts
#'     \item **responses**: Raw response data for completed prompts
#'     \item **completed**: Number of successfully processed prompts
#'     \item **file**: Path where batch state is saved
#'     \item **type**: Type specification used for structured data
#'     \item **texts**: Function to extract text responses or structured data
#'     \item **chats**: Function to extract chat objects
#'     \item **progress**: Function to get processing status
#'     \item **process**: Function to process a lot of prompts
#'   }
#' @section Process Method:
#' This function provides access to the `process()` method for sequential processing of prompts.
#' See `?process.sequential_chat` for full details of the method and its parameters.
#'
#' @examplesIf ellmer::has_credentials("openai")
#' # Create chat processor
#' chat <- seq_chat("openai/gpt-4.1")
#'
#' # Process prompts
#' response <- chat$process(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse",
#'     "Explain vectors, lists, and data frames"
#'   )
#' )
#'
#'
#' # Return responses
#' response$texts()
#'
#' # Return chat objects
#' response$chats()
#'
#' # Check progress if interrupted
#' response$progress()
#' @export
seq_chat <- function(
    chat_model = NULL,
    ...) {
  if (is.null(chat_model)) {
    cli::cli_abort("Define a chat model (e.g., {.code seq_chat('anthropic')})")
  }

  if (!is.character(chat_model) || length(chat_model) != 1) {
    cli::cli_abort("chat_model must be a character string (e.g., 'anthropic')")
  }

  chat_env <- create_chat_env(chat_model, ...)

  chat_env$process <- function(prompts,
                               type = NULL,
                               file = tempfile("chat_", fileext = ".rds"),
                               progress = TRUE,
                               beep = TRUE,
                               echo = FALSE,
                               ...) {
    file <- handle_file_persistence(chat_env, file)

    process.sequential_chat(
      chat_env = chat_env,
      prompts = prompts,
      type = type,
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
#' Process a lot of chat prompts in parallel using
#' \href{https://www.futureverse.org}{future} workers (multisession).
#' For persistent caching, save each chat to the disk and
#' resume processing from the last saved chat.
#' Use this function to process a lot of chat prompts very quickly.
#' For sequential processing, use `seq_chat()`.
#'
#' @param chat_model Character string specifying the chat model to use (e.g., "openai/gpt-4.1" or "anthropic/claude-3-5-sonnet-latest").
#'   This creates an ellmer chat object using [ellmer::chat()].
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return A process object (S7 class) containing:
#'   \itemize{
#'     \item **prompts**: Original input prompts
#'     \item **responses**: Raw response data for completed prompts
#'     \item **completed**: Number of successfully processed prompts
#'     \item **file**: Path where batch state is saved
#'     \item **type**: Type specification used for structured data
#'     \item **texts**: Function to extract text responses or structured data
#'     \item **chats**: Function to extract chat objects
#'     \item **progress**: Function to get processing status
#'     \item **process**: Function to process a lot of prompts
#'   }
#' @section Process Method:
#' This function provides access to the `process()` method for parallel processing of prompts.
#' See `?process.future_chat` for full details of the method and its parameters.
#'
#' @examplesIf interactive() && ellmer::has_credentials("openai")
#' # Create chat processor
#' chat <- future_chat("openai/gpt-4.1")
#'
#' # Process prompts
#' response <- chat$process(
#'   list(
#'     "What is R?",
#'     "Explain base R versus tidyverse",
#'     "Explain vectors, lists, and data frames"
#'   )
#' )
#'
#' # Return responses
#' response$texts()
#'
#' # Return chat objects
#' response$chats()
#'
#' # Check progress if interrupted
#' response$progress()
#' @export
future_chat <- function(
    chat_model = NULL,
    ...) {
  if (is.null(chat_model)) {
    cli::cli_abort("Define a chat model (e.g., {.code future_chat('anthropic')})")
  }

  if (!is.character(chat_model) || length(chat_model) != 1) {
    cli::cli_abort("chat_model must be a character string (e.g., 'anthropic')")
  }

  chat_env <- create_chat_env_deferred(chat_model, ...)

  chat_env$process <- function(prompts,
                               type = NULL,
                               file = tempfile("chat_", fileext = ".rds"),
                               progress = TRUE,
                               workers = NULL,
                               beep = TRUE,
                               echo = FALSE,
                               ...) {
    file <- handle_file_persistence(chat_env, file)

    if (is.null(workers)) {
      if (length(prompts) <= parallel::detectCores()) {
        workers <- length(prompts)
      } else {
        workers <- parallel::detectCores()
      }
    }

    process.future_chat(
      chat_env = chat_env,
      prompts = prompts,
      type = type,
      file = file,
      workers = workers,
      beep = beep,
      progress = progress,
      echo = echo,
      ...
    )
  }

  class(chat_env) <- c("future_chat", "Chat", "R6")
  chat_env
}
