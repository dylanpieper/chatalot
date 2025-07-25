#' Process a lot of prompts in sequence
#' @description
#' Process a lot of chat prompts in sequence, or one at a time.
#' Save responses to disk for each chat
#' and resume processing from the last saved chat.
#' For parallel processing, use `future_chat()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `chat_openai()`)
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
#' chat <- seq_chat(chat_openai(system_prompt = "Reply concisely"))
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
  validate_chat_model(chat_model)

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
#' Process a lot of chat prompts in parallel using multisession
#' \href{https://www.futureverse.org}{future} workers.
#' Split prompts into chunks to distribute across workers to process chats.
#' Save chat data to disk between chunks
#' and resume processing from the last saved chunk.
#' For sequential processing, use `seq_chat()`.
#'
#' @param chat_model ellmer chat model object or function (e.g., `chat_openai()`)
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
#' chat <- future_chat(chat_openai(system_prompt = "Reply concisely"))
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
  if (is_new_ellmer()) {
    cli::cli_abort(c(
      "`future_chat()` currently does not work in ellmer {packageVersion('ellmer')}",
      "!" = "This issue is temporary and will be fixed in a future ellmer release",
      "v" = "`future_chat()` will work if you install {.code pak::pak('ellmer@0.2.0')}",
      "!" = "Be aware that ellmer 0.2.0 exposes API keys in chat objects"
    ))
  }

  if (is.null(chat_model)) {
    cli::cli_abort("Define an ellmer chat_model (e.g., chat_openai)")
  }

  chat_env <- create_chat_env(chat_model, ...)

  chat_env$process <- function(prompts,
                               type = NULL,
                               file = tempfile("chat_", fileext = ".rds"),
                               progress = TRUE,
                               workers = NULL,
                               chunk_size = 10,
                               max_chunk_attempts = 3L,
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
