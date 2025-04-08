#' Process a batch of prompts with a sequential chat
#'
#' @param chat_env The chat environment from chat_sequential
#' @param prompts List of prompts to process
#' @param type_spec Type specification for structured data extraction
#' @param judgements Number of judgements (1 = initial extract + 1 judgement, 2 = initial extract + 2 judgements, etc.)
#' @param state_path Path to save state file
#' @param progress Whether to show progress bars
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param initial_delay Initial delay before first retry in seconds
#' @param max_delay Maximum delay between retries in seconds
#' @param backoff_factor Factor to multiply delay by after each retry
#' @param beep Whether to play a sound on completion
#' @param echo Whether to display chat outputs (when `progress` is `FALSE`)
#' @param ... Additional arguments passed to the chat method
#'
#' @return A batch object with the processed results
#' @export
batch.sequential_chat <- function(chat_env,
                                  prompts,
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

  process_sequential(
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

#' Process a batch of prompts with a parallel chat
#'
#' @param chat_env The chat environment from chat_future
#' @param prompts List of prompts to process
#' @param type_spec Type specification for structured data extraction
#' @param judgements Number of judgements for structured data extraction resulting in refined data
#' @param state_path Path to save state file
#' @param workers Number of parallel workers
#' @param chunk_size Number of prompts each worker processes at a time
#' @param plan Parallel backend ("multisession" or "multicore")
#' @param max_chunk_attempts Maximum retries per failed chunk
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param initial_delay Initial delay before first retry in seconds
#' @param max_delay Maximum delay between retries in seconds
#' @param backoff_factor Factor to multiply delay by after each retry
#' @param progress Whether to show progress bars
#' @param beep Whether to play a sound on completion
#' @param echo Whether to display chat outputs (when `progress` is `FALSE`)
#' @param ... Additional arguments passed to the chat method
#'
#' @return A batch object with the processed results
#' @export
batch.future_chat <- function(chat_env,
                              prompts,
                              type_spec = NULL,
                              judgements = 0,
                              state_path = tempfile("chat_", fileext = ".rds"),
                              workers = NULL,
                              chunk_size = parallel::detectCores() * 5,
                              plan = "multisession",
                              max_chunk_attempts = 3L,
                              max_retries = 3L,
                              initial_delay = 20,
                              max_delay = 80,
                              backoff_factor = 2,
                              beep = TRUE,
                              progress = TRUE,
                              echo = FALSE,
                              ...) {
  plan <- match.arg(plan, choices = c("multisession", "multicore"))

  if (judgements > 0 && is.null(type_spec)) {
    cli::cli_alert_warning("Judgements parameter ({judgements}) specified but will be ignored without a type_spec")
  }

  if (!is.null(type_spec) && judgements < 0) {
    cli::cli_abort("Number of judgements must be non-negative")
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
