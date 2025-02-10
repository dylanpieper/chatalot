#' @export
texts <- S7::new_generic("texts", "x")

#' @export
chats <- S7::new_generic("chats", "x")

#' @export
progress <- S7::new_generic("progress", "x")

#' @export
structured_data <- S7::new_generic("structured_data", "x")

#' @keywords internal
#' @import ellmer
#' @import S7
#' @importFrom purrr map_lgl walk map map_chr
#' @importFrom cli cli_progress_bar cli_progress_done
#' @importFrom cli cli_alert_success cli_alert_warning cli_h3 cli_text col_green
"_PACKAGE"

#' Batch class for managing chat processing
#' @name batch
#' @export
batch <- S7::new_class(
  "batch",
  properties = list(
    prompts = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        if (length(value) == 0) {
          "@prompts must not be empty"
        }
        if (!all(purrr::map_lgl(value, is.character))) {
          "@prompts must be a list of character strings"
        }
        NULL
      }
    ),
    responses = S7::new_property(class = S7::class_list, validator = function(value) NULL),
    completed = S7::new_property(
      class = S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          "@completed must be a single integer"
        }
        if (value < 0) {
          "@completed must be non-negative"
        }
        NULL
      }
    ),
    state_path = S7::class_character | NULL,
    type_spec = S7::new_property(
      class = S7::class_any | NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!inherits(value, c("ellmer::TypeObject", "ellmer::Type", "ellmer::TypeArray"))) {
            return("@type_spec must be an ellmer type specification (created with type_object(), type_array(), etc.) or NULL")
          }
        }
        NULL
      }
    ),
    echo = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (!value %in% c("none", "text", "all")) {
          "@echo must be one of 'none', 'text', or 'all'"
        }
        NULL
      }
    ),
    progress_bar = S7::new_property(class = S7::class_any | NULL),
    input_type = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (!value %in% c("vector", "list")) {
          "input_type must be either 'vector' or 'list'"
        } else {
          NULL
        }
      }
    )
  ),
  validator = function(self) {
    if (self@completed > length(self@prompts)) {
      "@completed cannot be larger than number of prompts"
    }
    NULL
  }
)

#' Extract structured data from a batch
#' @name structured_data.batch
#' @param x A batch object
#' @return List of structured data
#' @importFrom cli cli_alert_warning
#' @importFrom purrr map
S7::method(structured_data, batch) <- function(x) {
  if (is.null(x@type_spec)) {
    cli_alert_warning("No type specification provided for structured data extraction")
    return(NULL)
  }
  responses <- x@responses[seq_len(x@completed)]
  map(responses, "structured_data")
}

#' Extract text responses from a batch
#' @name texts.batch
#' @param x A batch object
#' @return A character vector (if original prompts were supplied as a vector) or
#'   a list of response texts (if original prompts were supplied as a list)
#' @importFrom purrr map_chr
S7::method(texts, batch) <- function(x) {
  responses <- x@responses[seq_len(x@completed)]
  text_values <- purrr::map_chr(responses, "text")
  
  if (x@input_type == "vector") {
    return(text_values)
  } else {
    return(as.list(text_values))
  }
}

#' Extract chat objects from a batch
#' @name chats.batch
#' @param x A batch object
#' @return List of chat objects
#' @importFrom purrr map
S7::method(chats, batch) <- function(x) {
  responses <- x@responses[seq_len(x@completed)]
  map(responses, "chat")
}

#' Capture chat model response with proper handling
#' @param original_chat Original chat model object
#' @param prompt Prompt text
#' @param type_spec Type specification for structured data
#' @param echo Echo level ("none", "text", "all")
#' @return List containing response information
#' @keywords internal
capture <- function(original_chat, prompt, type_spec = NULL, echo = "text") {
  response <- NULL
  structured_data <- NULL
  chat <- original_chat$clone()
  
  if (echo == "all") {
    cli::cli_h3(cli::col_green("Prompt"))
    cli::cli_text(prompt)
    cli::cli_h3(cli::col_green("Response"))
  }
  
  if (!is.null(type_spec)) {
    if (echo == "none") {
      temp_file <- tempfile()
      sink(temp_file)
      structured_data <- chat$extract_data(prompt, type = type_spec)
      sink()
      unlink(temp_file)
    } else {
      structured_data <- chat$extract_data(prompt, type = type_spec)
      str(structured_data)
    }
  } else {
    output <- utils::capture.output(response <- chat$chat(prompt))
    if (echo %in% c("all", "text")) {
      cli::cli_text(paste(output, collapse = "\n"))
    }
  }
  
  list(
    chat = chat,
    text = response,
    structured_data = structured_data,
    turns = chat$get_turns(),
    tokens = chat$tokens()
  )
}

#' Process batch of prompts with progress tracking
#' @param chat_obj Chat model object
#' @param prompts List of prompts
#' @param type_spec Type specification for structured data
#' @param state_path Path for saving state
#' @param echo Echo level ("none", "text", or "all")
#' @return Batch results object
#' @export
process <- function(chat_obj, prompts, type_spec = NULL,
                    state_path = tempfile("chat_batch_", fileext = ".rds"),
                    echo = "text") {
  
  if (file.exists(state_path)) {
    result <- readRDS(state_path)
    if (!identical(as.list(prompts), result@prompts)) {
      unlink(state_path)
      result <- NULL
    }
  } else {
    result <- NULL
  }
  
  if (is.null(result)) {
    orig_type <- if (is.atomic(prompts) && !is.list(prompts)) "vector" else "list"
    
    result <- batch(
      prompts = as.list(prompts),
      responses = vector("list", length(prompts)),
      completed = 0L,
      state_path = state_path,
      type_spec = type_spec,
      echo = echo,
      input_type = orig_type
    )
  }
  
  total_prompts <- length(prompts)
  
  if (result@completed >= total_prompts) {
    if (echo == "none") {
      cli::cli_alert_success("Complete")
    }
    return(create_results(result))
  }
  
  if (echo == "none") {
    progress_bar <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Processing chats [{cli::pb_current}/{cli::pb_total}] ",
        "[{cli::pb_bar}] {cli::pb_percent}"
      ),
      total = total_prompts,
      clear = TRUE
    )
    cli::cli_progress_update(id = progress_bar, set = result@completed)
  }
  
  remaining_chats <- (result@completed + 1L):total_prompts
  
  tryCatch({
    for (i in remaining_chats) {
      if (echo %in% c("text", "all")) {
        cli::cli_h3(cli::col_green(sprintf("Processing chats [%d/%d]", i, total_prompts)))
      }
      
      response <- capture(chat_obj, prompts[[i]], type_spec, echo)
      result@responses[[i]] <- response
      result@completed <- i
      saveRDS(result, state_path)
      
      if (echo == "none") {
        cli::cli_progress_update(id = progress_bar, set = i)
      }
    }
    
    if (echo == "none") {
      cli::cli_alert_success("Complete")
      cli::cli_progress_done(id = progress_bar)
    }
    
  }, error = function(e) {
    saveRDS(result, state_path)
    if (echo == "none") {
      cli::cli_progress_done(id = progress_bar)
      cli::cli_alert_warning(
        paste0("Interrupted at ", result@completed, " of ", total_prompts, " prompts")
      )
    }
    if (!inherits(e, "interrupt")) {
      stop(e)
    }
  }, interrupt = function(e) {
    saveRDS(result, state_path)
    if (echo == "none") {
      cli::cli_progress_done(id = progress_bar)
      cli::cli_alert_warning(
        paste0("Interrupted at ", result@completed, " of ", total_prompts, " prompts")
      )
    }
  })
  
  create_results(result)
}

#' Create results object from batch
#' @param result Batch object
#' @return Results object with class "batch"
#' @keywords internal
create_results <- function(result) {
  base_list <- list(
    prompts = result@prompts,
    responses = result@responses,
    completed = result@completed,
    state_path = result@state_path,
    type_spec = result@type_spec
  )
  
  base_list$texts <- function() texts(result)
  base_list$chats <- function() chats(result)
  base_list$progress <- function() progress(result)
  base_list$structured_data <- function() structured_data(result)
  
  structure(base_list, class = "batch")
}

#' Process batch of prompts
#' @param chat_obj Chat model object
#' @param prompts List of prompts
#' @param type_spec Type specification for structured data
#' @param state_path Path for saving state
#' @param echo Echo level
#' @return Batch results object
#' @export
chat_batch <- function(chat_model = chat_openai(), echo = "text", ...) {
  original_chat <- chat_model
  
  chat_env <- new.env(parent = emptyenv())
  
  walk(
    names(original_chat),
    ~ assign(.x, original_chat[[.x]], envir = chat_env)
  )
  
  chat_env$last_state_path <- NULL
  
  chat_env$batch <- function(prompts,
                             type_spec = NULL,
                             state_path = tempfile("chat_batch_", fileext = ".rds")) {
    if (is.null(chat_env$last_state_path)) {
      chat_env$last_state_path <- state_path
    } else {
      state_path <- chat_env$last_state_path
    }
    
    process(original_chat, prompts, type_spec, state_path, echo)
  }
  
  structure(chat_env, class = class(original_chat))
}