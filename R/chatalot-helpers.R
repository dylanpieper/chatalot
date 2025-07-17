validate_chat_model <- function(chat_model) {
  if (is.null(chat_model)) {
    stop("Define an ellmer chat model (e.g., chat_openai)")
  }
}

create_chat_env <- function(chat_model, ...) {
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
  
  chat_env
}

handle_file_persistence <- function(chat_env, file) {
  if (is.null(chat_env$last_file)) {
    chat_env$last_file <- file
  } else {
    file <- chat_env$last_file
  }
  file
}