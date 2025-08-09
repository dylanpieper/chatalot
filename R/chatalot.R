#' Create a sequential chat processor
#' @description
#' Access methods to process lots of chat prompts in sequence, or one at a time.
#' Use this function to process prompts slowly, such as when providers don't allow parallel processing
#' or have strict rate limits, or when you want to periodically check the responses.
#' For parallel processing, use `future_chat()`.
#'
#' @param chat_model Character string specifying the chat model to use (e.g., "openai/gpt-4.1" or "anthropic/claude-3-5-sonnet-latest").
#'   This creates an ellmer chat object using [ellmer::chat()].
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return An R6 object with functions:
#'   \itemize{
#'     \item `$process()`: Function to process multiple prompts sequentially.
#'       Takes a vector or list of prompts and processes them one by one with persistent caching.
#'       Returns a process object containing results and helper functions.
#'       See `?process.seq_chat` for full details of the method and its parameters.
#'     \item `$register_tool()`: Function to register tools that call functions to be used
#'       during chat interactions. Works the same as ellmer's [`$register_tool()`](https://ellmer.tidyverse.org/articles/tool-calling.html).
#'   }
#'
#' @examplesIf ellmer::has_credentials("openai")
#' # Create chat processor
#' chat <- seq_chat("openai/gpt-4.1")
#'
#' # Process prompts
#' response <- chat$process(
#'   c(
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

    process.seq_chat(
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

#' Create a parallel chat processor
#' @description
#' Access methods to process lots of chat prompts in parallel using multisession
#' \href{https://www.futureverse.org}{future} workers.
#' Use this function to process lots of chat prompts simultaneously and quickly.
#' For sequential processing, use `seq_chat()`.
#'
#' @param chat_model Character string specifying the chat model to use (e.g., "openai/gpt-4.1" or "anthropic/claude-3-5-sonnet-latest").
#'   This creates an ellmer chat object using [ellmer::chat()].
#' @param ... Additional arguments passed to the underlying chat model (e.g., `system_prompt`)
#' @return An R6 object with functions:
#'   \itemize{
#'     \item `$process()`: Function to process multiple prompts in parallel.
#'       Takes a vector or list of prompts and processes them simultaneously
#'       using multiple workers with persistent caching. Returns a process object
#'       containing results and helper functions. See `?process.future_chat` for full details
#'       of the method and its parameters.
#'     \item `$register_tool()`: Function to register tools that call functions to be used
#'       during chat interactions. Works the same as ellmer's [`$register_tool()`](https://ellmer.tidyverse.org/articles/tool-calling.html).
#'   }
#'
#' @examplesIf interactive() && ellmer::has_credentials("openai")
#' # Create chat processor
#' chat <- future_chat("openai/gpt-4.1")
#'
#' # Process prompts
#' response <- chat$process(
#'   c(
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
