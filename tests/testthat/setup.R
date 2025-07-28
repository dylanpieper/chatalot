skip_chat_test <- function() {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")
}

create_test_chat <- function(chat_constructor, model = "openai/gpt-4.1", system_prompt = "Reply concisely, one sentence") {
  chat_constructor(model, system_prompt = system_prompt)
}

get_test_prompts <- function(n = 3) {
  prompts <- list(
    "What is 2+2?",
    "What color is the sky?"
  )
  return(prompts[seq_len(n)])
}

get_content_prompts <- function() {
  base_prompt <- "What do you see in the image?"
  list(
    c(content_image_url("https://www.r-project.org/Rlogo.png"), base_prompt),
    c(content_image_file(system.file("httr2.png", package = "ellmer")), base_prompt)
  )
}

get_tool_prompts <- function() {
  list(
    "What is the square of 3?",
    "Calculate the square of 5."
  )
}

get_sentiment_type_spec <- function() {
  type_object(
    "Extract sentiment",
    score = type_number("Sentiment score from -1 to 1")
  )
}

get_square_tool <- function() {
  square_number <- function(num) num^2
  tool(
    fun = square_number,
    description = "Calculates the square of a number",
    arguments = list(
      num = type_integer("The number to square")
    )
  )
}

validate_process_result <- function(result, expected_length) {
  expect_type(result, "list")
  expect_s3_class(result, "process")
  expect_false(is.null(result$texts()))
  expect_type(result$texts(), "list")
  expect_type(result$texts()[[1]], "character")
  expect_equal(length(result$texts()), expected_length)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, c("Chat", "R6")))))
}

get_sentiment_prompts <- function() {
  list(
    "I love this!",
    "This is terrible."
  )
}

validate_structured_data <- function(data, expected_rows) {
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), expected_rows)
  expect_true(all(!is.na(data$score)))
}

test_api_key_failure <- function(chat_constructor, processor_args = list()) {
  original_key <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  Sys.unsetenv("OPENAI_API_KEY")
  Sys.setenv(OPENAI_API_KEY = "invalid_key")

  chat <- create_test_chat(chat_constructor)

  on.exit({
    if (!is.na(original_key)) {
      Sys.setenv(OPENAI_API_KEY = original_key)
    } else {
      Sys.unsetenv("OPENAI_API_KEY")
    }
  })

  args <- c(list(get_test_prompts(1), beep = FALSE), processor_args)
  expect_error(
    do.call(chat$process, args),
    regexp = NULL
  )
}

create_temp_file_test <- function(chat, processor_args = list()) {
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))

  args <- c(list(get_test_prompts(1), file = temp_file, beep = FALSE), processor_args)
  result <- do.call(chat$process, args)

  expect_true(file.exists(temp_file))
  validate_process_result(result, 1)
}

test_progress_parameter <- function(chat_constructor, processor_args = list()) {
  chat <- create_test_chat(chat_constructor)
  args_true <- c(list(get_test_prompts(1), progress = TRUE, beep = FALSE), processor_args)
  result <- do.call(chat$process, args_true)
  validate_process_result(result, 1)

  chat <- create_test_chat(chat_constructor)
  args_false <- c(list(get_test_prompts(1), progress = FALSE, beep = FALSE), processor_args)
  result <- do.call(chat$process, args_false)
  validate_process_result(result, 1)
}

test_echo_parameter <- function(chat_constructor, processor_args = list()) {
  chat <- create_test_chat(chat_constructor)
  args <- c(list(get_test_prompts(1), progress = FALSE, echo = TRUE, beep = FALSE), processor_args)
  result <- do.call(chat$process, args)
  validate_process_result(result, 1)
}
