test_that("chat_parallel initialization works", {
  chat <- chat_parallel()
  expect_s3_class(chat, "ellmer_chat")
  expect_type(chat$batch, "closure")
})

test_that("chat_parallel processes chunks correctly", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel(workers = 2)
  result <- chat$batch(get_test_prompts(2), chunk_size = 1)
  
  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 2)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, "ellmer_chat"))))
})

test_that("chat_parallel handles structured data", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel()
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

test_that("chat_parallel works with tools", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel()
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

test_that("chat_parallel handles state persistence", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  chat <- chat_parallel()
  result <- chat$batch(get_test_prompts(1), state_path = temp_file, chunk_size = 1)
  
  expect_true(file.exists(temp_file))
  expect_equal(length(result$texts()), 1)
})

test_that("chat_parallel respects timeout", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel(timeout = 30)
  result <- chat$batch(get_test_prompts(1), chunk_size = 1)
  expect_equal(length(result$texts()), 1)
})

test_that("chat_parallel supports echo", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel(echo = "text")
  result <- chat$batch(get_test_prompts(1), chunk_size = 1)
  expect_equal(length(result$texts()), 1)
})

test_that("chat_parallel handles worker failures", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel()
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "invalid_key")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))
  
  expect_error(
    chat$batch(get_test_prompts(1), chunk_size = 1),
    regexp = "auth|unauthorized|invalid.*key",
    ignore.case = TRUE
  )
})

test_that("chat_parallel handles interruption and resume", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  chat <- create_interruptible_chat(chat_parallel, after_n_calls = 1)
  expect_error(
    chat$batch(get_test_prompts(3), state_path = temp_file, chunk_size = 1),
    class = "interrupt"
  )
  
  expect_true(file.exists(temp_file))
  
  chat_resume <- chat_parallel()
  result <- chat_resume$batch(get_test_prompts(3), state_path = temp_file, chunk_size = 1)
  
  expect_equal(length(result$texts()), 3)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, "ellmer_chat"))))
})