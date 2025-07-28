#' Create a chat environment
#'
#' Creates a new environment for managing chat interactions. Handles both
#' new string-based API and legacy function-based API for backward compatibility.
#'
#' @param chat_model Either a string model name, a function, or a chat object
#' @param ... Additional arguments passed to the chat model constructor
#' @return A new environment containing the chat model and related components
#' @keywords internal
#' @noRd
create_chat_env <- function(chat_model, ...) {
  chat_env <- new.env(parent = emptyenv())

  if (is.character(chat_model)) {
    chat_env$chat_model <- ellmer::chat(chat_model, ...)
  } else if (is.function(chat_model)) {
    chat_env$chat_model <- chat_model(...)
  } else {
    chat_env$chat_model <- chat_model
  }

  for (n in names(chat_env$chat_model)) {
    chat_env[[n]] <- chat_env$chat_model[[n]]
  }

  chat_env$last_file <- NULL

  chat_env
}

#' Create a deferred chat environment
#'
#' Creates a chat environment with deferred model initialization.
#' The actual chat model is not created until needed.
#'
#' @param chat_model Model name or specification
#' @param ... Additional arguments to store for deferred model creation
#' @return A new environment with stored model parameters
#' @keywords internal
#' @noRd
create_chat_env_deferred <- function(chat_model, ...) {
  chat_env <- new.env(parent = emptyenv())

  chat_env$chat_model_name <- chat_model
  chat_env$chat_model_args <- list(...)
  chat_env$last_file <- NULL

  chat_env
}


#' Handle file persistence in chat environment
#'
#' Manages file persistence by storing the first file provided and
#' using it for subsequent calls when no file is specified.
#'
#' @param chat_env The chat environment object
#' @param file The file path to handle
#' @return The file path to use (either provided or previously stored)
#' @keywords internal
#' @noRd
handle_file_persistence <- function(chat_env, file) {
  if (is.null(chat_env$last_file)) {
    chat_env$last_file <- file
  } else {
    file <- chat_env$last_file
  }
  file
}
