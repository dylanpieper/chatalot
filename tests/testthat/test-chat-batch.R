test_that("chat_batch initialization works", {
  chat <- chat_batch(beep = FALSE)
  expect_true(inherits(chat, "Chat"))
  expect_true(inherits(chat, "R6"))
  expect_type(chat$batch, "closure")
})

test_that("chat_batch processes prompts correctly", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch(beep = FALSE)
  result <- chat$batch(get_test_prompts(2))
  
  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 2)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, c("Chat", "R6")))))
})

test_that("chat_batch handles structured data", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch(beep = FALSE)
  prompts <- list(
    "I love this!",
    "This is terrible."
  )
  
  result <- chat$batch(prompts, type_spec = get_sentiment_type_spec())
  data <- result$structured_data()
  
  expect_type(data, "list")
  expect_length(data, 2)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("chat_batch works with tools", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch(beep = FALSE)
  chat$register_tool(get_square_tool())
  
  result <- chat$batch(list(
    "What is the square of 3?",
    "Calculate the square of 5."
  ))
  
  expect_equal(length(result$texts()), 2)
})

test_that("chat_batch handles state persistence", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  chat <- chat_batch(beep = FALSE)
  result <- chat$batch(get_test_prompts(1), state_path = temp_file)
  
  expect_true(file.exists(temp_file))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_batch respects timeout", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch(timeout = 30, beep = FALSE)
  result <- chat$batch(get_test_prompts(1))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_batch supports echo", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch(echo = "text", beep = FALSE)
  result <- chat$batch(get_test_prompts(1))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_batch handles errors gracefully", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch(beep = FALSE)
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "invalid_key")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))
  
  expect_error(
    chat$batch(get_test_prompts(1)),
    regexp = "HTTP 401 Unauthorized|invalid.*api.*key|authentication_error",
    ignore.case = TRUE
  )
})