#' Process a batch of prompts with a sequential chat
#'
#' @param chat_env The chat environment from chat_sequential
#' @param prompts List of prompts to process
#' @param type Type specification for structured data extraction
#' @param eval If TRUE, performs one evaluation round for structured data extraction (default: FALSE)
#' @param file Path to save state file (.rds)
#' @param progress Whether to show progress bars
#' @param beep Whether to play a sound on completion
#' @param echo Whether to display chat outputs (when `progress` is `FALSE`)
#' @param ... Additional arguments passed to the chat method
#'
#' @return A batch object with the processed results
#' @export
batch.sequential_chat <- function(chat_env,
                                  prompts,
                                  type = NULL,
                                  eval = FALSE,
                                  file = tempfile("chat_", fileext = ".rds"),
                                  progress = TRUE,
                                  beep = TRUE,
                                  echo = FALSE,
                                  ...) {
  if (eval && is.null(type)) {
    cli::cli_alert_warning("eval parameter specified but will be ignored without a type")
  }

  if (!is.null(type) && !is.logical(eval)) {
    cli::cli_abort("eval parameter must be logical (TRUE/FALSE)")
  }

  process_sequential(
    chat_obj = chat_env$chat_model,
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

#' Process a batch of prompts with a parallel chat
#'
#' @param chat_env The chat environment from chat_future
#' @param prompts List of prompts to process
#' @param type Type specification for structured data extraction
#' @param eval If TRUE, performs one evaluation round for structured data extraction (default: FALSE)
#' @param file Path to save state file
#' @param workers Number of parallel workers (default upper limit is `parallel::detectCores()`)
#' @param chunk_size Number of prompts each worker processes at a time
#' @param max_chunk_attempts Maximum retries per failed chunk
#' @param progress Whether to show progress bars
#' @param beep Whether to play a sound on completion
#' @param echo Whether to display chat outputs (when `progress` is `FALSE`)
#' @param ... Additional arguments passed to the chat method
#'
#' @return A batch object with the processed results
#' @export
batch.future_chat <- function(chat_env,
                              prompts,
                              type = NULL,
                              eval = FALSE,
                              file = tempfile("chat_", fileext = ".rds"),
                              workers = NULL,
                              chunk_size = parallel::detectCores(),
                              max_chunk_attempts = 3L,
                              beep = TRUE,
                              progress = TRUE,
                              echo = FALSE,
                              ...) {

  if (eval && is.null(type)) {
    cli::cli_alert_warning("eval parameter specified but will be ignored without a type")
  }

  if (!is.null(type) && !is.logical(eval)) {
    cli::cli_abort("eval parameter must be logical (TRUE/FALSE)")
  }

  process_future(
    chat_obj = chat_env$chat_model,
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
