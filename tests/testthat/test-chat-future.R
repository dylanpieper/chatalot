test_that("chat_future initialization and result class works", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_future(ellmer::chat_openai)
  expect_true(inherits(chat, "Chat"))
  expect_true(inherits(chat, "R6"))

  result <- chat$batch(get_test_prompts(1), beep = FALSE)
  expect_true(inherits(result$chats()[[1]], c("Chat", "R6")))
})

test_that("chat_future processes chunks correctly", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_future(ellmer::chat_openai)
  result <- chat$batch(get_test_prompts(2), workers = 1, chunk_size = 1, beep = FALSE)

  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 2)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, c("Chat", "R6")))))
})

test_that("chat_future handles structured data extraction", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_future(ellmer::chat_openai)
  prompts <- list(
    "I love this!",
    "This is terrible."
  )

  result <- chat$batch(prompts, type_spec = get_sentiment_type_spec(), workers = 1, chunk_size = 1, beep = FALSE)
  data <- result$texts()

  expect_type(data, "list")
  expect_length(data, 2)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("chat_future handles structured data with judgements", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_future(ellmer::chat_openai)
  prompts <- list(
    "I love this!",
    "This is terrible."
  )

  result <- chat$batch(prompts, type_spec = get_sentiment_type_spec(), chunk_size = 1, judgements = 1, workers = 1, beep = FALSE)
  data <- result$texts()
  turns <- result$chats()[[1]]$get_turns()

  expect_type(data, "list")
  expect_length(data, 2)
  expect_length(turns, 6)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("chat_future works with tools", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_future(ellmer::chat_openai)
  chat$register_tool(get_square_tool())

  result <- chat$batch(
    list(
      "What is the square of 3?",
      "Calculate the square of 5."
    ),
    workers = 1,
    chunk_size = 1,
    beep = FALSE
  )

  expect_equal(length(result$texts()), 2)
})

test_that("chat_future handles state persistence", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))

  chat <- chat_future(ellmer::chat_openai)
  result <- chat$batch(get_test_prompts(1), workers = 1, state_path = temp_file, chunk_size = 1, beep = FALSE)

  expect_true(file.exists(temp_file))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_future handles worker failures", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  original_key <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  Sys.unsetenv("OPENAI_API_KEY")
  Sys.setenv(OPENAI_API_KEY = "invalid_key")

  chat <- chat_future(ellmer::chat_openai)

  on.exit({
    if (!is.na(original_key)) {
      Sys.setenv(OPENAI_API_KEY = original_key)
    } else {
      Sys.unsetenv("OPENAI_API_KEY")
    }
  })

  expect_error(
    chat$batch(get_test_prompts(1), workers = 1, chunk_size = 1, beep = FALSE),
    regexp = NULL
  )
})

test_that("chat_future supports progress parameter", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  # Test with progress = TRUE
  chat <- chat_future(ellmer::chat_openai)
  result <- chat$batch(get_test_prompts(1), workers = 1, chunk_size = 1, progress = TRUE, beep = FALSE)
  expect_equal(length(result$texts()), 1)

  # Test with progress = FALSE
  chat <- chat_future(ellmer::chat_openai)
  result <- chat$batch(get_test_prompts(1), workers = 1, chunk_size = 1, progress = FALSE, beep = FALSE)
  expect_equal(length(result$texts()), 1)
})

test_that("chat_future supports echo parameter and passes extra args", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_future(ellmer::chat_openai)

  # Test with echo = TRUE
  result <- chat$batch(get_test_prompts(1), workers = 1, chunk_size = 1, progress = FALSE, echo = TRUE, beep = FALSE)
  expect_equal(length(result$texts()), 1)

  # Test with additional parameter
  result <- chat$batch(get_test_prompts(1),
    workers = 1,
    chunk_size = 1,
    progress = FALSE,
    echo = TRUE,
    beep = FALSE
  )
  expect_equal(length(result$texts()), 1)
})
