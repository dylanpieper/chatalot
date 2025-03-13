test_that("chat_sequential initialization and result class works", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)
  expect_true(inherits(chat, "Chat"))
  expect_true(inherits(chat, "R6"))

  result <- chat$batch(get_test_prompts(1))
  expect_true(inherits(result$chats()[[1]], c("Chat", "R6")))
})

test_that("chat_sequential processes prompts correctly", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)
  result <- chat$batch(get_test_prompts(2))

  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 2)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, c("Chat", "R6")))))
})

test_that("chat_sequential handles structured data extraction", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)
  prompts <- list(
    "I love this!",
    "This is terrible."
  )

  result <- chat$batch(prompts, type_spec = get_sentiment_type_spec())
  data <- result$texts()

  expect_type(data, "list")
  expect_length(data, 2)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("chat_sequential handles structured data with judgements", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)
  prompts <- list(
    "I love this!",
    "This is terrible."
  )

  result <- chat$batch(prompts, type_spec = get_sentiment_type_spec(), judgements = 1)
  data <- result$texts()
  turns <- result$chats()[[1]]$get_turns()

  expect_type(data, "list")
  expect_length(data, 2)
  expect_length(turns, 6)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("chat_sequential works with tools", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)
  chat$register_tool(get_square_tool())

  result <- chat$batch(list(
    "What is the square of 3?",
    "Calculate the square of 5."
  ))

  expect_equal(length(result$texts()), 2)
})

test_that("chat_sequential handles state persistence", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)
  result <- chat$batch(get_test_prompts(1), state_path = temp_file)

  expect_true(file.exists(temp_file))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_sequential respects timeout", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, timeout = 30, beep = FALSE)
  result <- chat$batch(get_test_prompts(1))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_sequential supports echo", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  chat <- chat_sequential(ellmer::chat_openai, echo = "text", beep = FALSE)
  result <- chat$batch(get_test_prompts(1))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_sequential handles errors gracefully", {
  skip_if_not(ellmer::has_credentials("openai"), "API key not available")

  original_key <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  Sys.unsetenv("OPENAI_API_KEY")
  Sys.setenv(OPENAI_API_KEY = "invalid_key")

  chat <- chat_sequential(ellmer::chat_openai, beep = FALSE)

  on.exit({
    if (!is.na(original_key)) {
      Sys.setenv(OPENAI_API_KEY = original_key)
    } else {
      Sys.unsetenv("OPENAI_API_KEY")
    }
  })

  expect_error(
    chat$batch(get_test_prompts(1)),
    regexp = NULL
  )
})
