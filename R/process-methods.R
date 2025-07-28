#' Process a lot of prompts with a sequential chat
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

#' Process a lot of prompts with a parallel chat
#'
#' @param chat_env The chat environment from future_chat
#' @param prompts List of prompts to process
#' @param type Type specification for structured data extraction
#' @param file Path to save state file
#' @param workers Number of parallel workers (default upper limit is `parallel::detectCores()`)
#' @param chunk_size Number of prompts each worker processes at a time (default: 10)
#' @param max_chunk_attempts Maximum retries per failed chunk
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
                                chunk_size = 10,
                                max_chunk_attempts = 3L,
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
    chunk_size = chunk_size,
    max_chunk_attempts = max_chunk_attempts,
    beep = beep,
    progress = progress,
    echo = echo,
    ...
  )
}
