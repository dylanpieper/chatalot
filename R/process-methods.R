#' Process lots of chat prompts in sequence
#'
#' @param chat_env The chat environment from seq_chat
#' @param prompts List of prompts to process
#' @param type Type specification for structured data extraction
#' @param file Path to save state file (.rds)
#' @param progress Whether to show progress bars
#' @param beep Whether to play a sound on completion
#' @param echo Whether to display chat outputs (when `progress` is `FALSE`)
#' @param ... Additional arguments passed to the chat method
#'
#' @return A process object with the processed results
#' @export
process.sequential_chat <- function(chat_env,
                                    prompts,
                                    type = NULL,
                                    file = tempfile("chat_", fileext = ".rds"),
                                    progress = TRUE,
                                    beep = TRUE,
                                    echo = FALSE,
                                    ...) {
  process_sequential(
    chat_obj = chat_env$chat_model,
    prompts = prompts,
    type = type,
    file = file,
    progress = progress,
    beep = beep,
    echo = echo,
    ...
  )
}

#' Process lots of chat prompts in parallel
#'
#' @param chat_env The chat environment from future_chat
#' @param prompts List of prompts to process
#' @param type Type specification for structured data extraction
#' @param file Path to save state file
#' @param workers Number of parallel workers (default is `parallel::detectCores()`).
#'   You may want to limit the number of simultaneous requests to meet a provider's
#'   rate limits by decreasing this value.
#' @param progress Whether to show progress bars
#' @param beep Whether to play a sound on completion
#' @param echo Whether to display chat outputs (when `progress` is `FALSE`)
#' @param ... Additional arguments passed to the chat method
#'
#' @return A process object with the processed results
#' @export
process.future_chat <- function(chat_env,
                                prompts,
                                type = NULL,
                                file = tempfile("chat_", fileext = ".rds"),
                                workers = NULL,
                                beep = TRUE,
                                progress = TRUE,
                                echo = FALSE,
                                ...) {
  if (exists("chat_model", envir = chat_env)) {
    chat_obj <- chat_env$chat_model
  } else {
    chat_obj <- chat_env
  }

  process_future(
    chat_obj = chat_obj,
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
