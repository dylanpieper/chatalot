test_that("chat_future initialization and result class works", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  chat <- chat_future(ellmer::chat_claude, beep = FALSE)
  expect_true(inherits(chat, "Chat"))
  expect_true(inherits(chat, "R6"))

  result <- chat$batch(get_test_prompts(1))
  expect_true(inherits(result$chats()[[1]], c("Chat", "R6")))
})

test_that("chat_future processes chunks correctly", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  chat <- chat_future(ellmer::chat_claude, workers = 1, beep = FALSE)
  result <- chat$batch(get_test_prompts(2), chunk_size = 1)

  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 2)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, c("Chat", "R6")))))
})

test_that("chat_future handles structured data", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  chat <- chat_future(ellmer::chat_claude, workers = 1, beep = FALSE)
  prompts <- list(
    "I love this!",
    "This is terrible."
  )

  result <- chat$batch(prompts, type_spec = get_sentiment_type_spec(), chunk_size = 1)
  data <- result$structured_data()

  expect_type(data, "list")
  expect_length(data, 2)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("chat_future works with tools", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  chat <- chat_future(ellmer::chat_claude, workers = 1, beep = FALSE)
  chat$register_tool(get_square_tool())

  result <- chat$batch(
    list(
      "What is the square of 3?",
      "Calculate the square of 5."
    ),
    chunk_size = 1
  )

  expect_equal(length(result$texts()), 2)
})

test_that("chat_future handles state persistence", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))

  chat <- chat_future(ellmer::chat_claude, workers = 1, beep = FALSE)
  result <- chat$batch(get_test_prompts(1), state_path = temp_file, chunk_size = 1)

  expect_true(file.exists(temp_file))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_future respects timeout", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  chat <- chat_future(ellmer::chat_claude, workers = 1, timeout = 30, beep = FALSE)
  result <- chat$batch(get_test_prompts(1), chunk_size = 1)
  expect_equal(length(result$texts()), 1)
})

test_that("chat_future handles worker failures", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")

  original_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  Sys.unsetenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "invalid_key")

  chat <- chat_future(ellmer::chat_claude, workers = 1, beep = FALSE)

  on.exit({
    if (!is.na(original_key)) {
      Sys.setenv(ANTHROPIC_API_KEY = original_key)
    } else {
      Sys.unsetenv("ANTHROPIC_API_KEY")
    }
  })

  expect_error(
    chat$batch(get_test_prompts(1), chunk_size = 1),
    regexp = NULL
  )
})
