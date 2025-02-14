test_that("interruption preserves partial results", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  chat <- create_interruptible_chat(chat_batch, after_n_calls = 1)
  expect_error(
    chat$batch(get_test_prompts(3), state_path = temp_file),
    class = "interrupt"
  )
  
  saved_state <- readRDS(temp_file)
  expect_true(length(saved_state$results) > 0)
  
  chat_resume <- chat_batch()
  result <- chat_resume$batch(get_test_prompts(3), state_path = temp_file)
  expect_equal(length(result$texts()), 3)
  
  expect_equal(
    saved_state$results[[1]]$text,
    result$texts()[[1]]
  )
})

test_that("multiple interruptions are handled correctly", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  chat1 <- create_interruptible_chat(chat_batch, after_n_calls = 1)
  expect_error(
    chat1$batch(get_test_prompts(3), state_path = temp_file),
    class = "interrupt"
  )
  
  chat2 <- create_interruptible_chat(chat_batch, after_n_calls = 1)
  expect_error(
    chat2$batch(get_test_prompts(3), state_path = temp_file),
    class = "interrupt"
  )
  
  chat_final <- chat_batch()
  result <- chat_final$batch(get_test_prompts(3), state_path = temp_file)
  expect_equal(length(result$texts()), 3)
})

test_that("parallel interruption preserves chunk progress", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  chat <- create_interruptible_chat(chat_parallel, after_n_calls = 1)
  expect_error(
    chat$batch(get_test_prompts(6), state_path = temp_file, chunk_size = 2),
    class = "interrupt"
  )
  
  saved_state <- readRDS(temp_file)
  expect_true(length(saved_state$results) > 0)
  expect_true(length(saved_state$results) < 6)
  
  chat_resume <- chat_parallel()
  result <- chat_resume$batch(get_test_prompts(6), state_path = temp_file, chunk_size = 2)
  expect_equal(length(result$texts()), 6)
  
  expect_equal(
    saved_state$results[[1]]$text,
    result$texts()[[1]]
  )
})

test_that("interruption works with structured data", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  prompts <- list(
    "I love this!",
    "This is terrible.",
    "Feeling neutral."
  )
  
  chat <- create_interruptible_chat(chat_batch, after_n_calls = 1)
  expect_error(
    chat$batch(prompts, state_path = temp_file, type_spec = get_sentiment_type_spec()),
    class = "interrupt"
  )
  
  chat_resume <- chat_batch()
  result <- chat_resume$batch(prompts, state_path = temp_file, type_spec = get_sentiment_type_spec())
  data <- result$structured_data()
  
  expect_type(data, "list")
  expect_length(data, 3)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
})

test_that("interruption works with tools", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  prompts <- list(
    "What is the square of 3?",
    "Calculate the square of 5.",
    "What is 4 squared?"
  )
  
  chat <- create_interruptible_chat(chat_batch, after_n_calls = 1)
  chat$register_tool(get_square_tool())
  expect_error(
    chat$batch(prompts, state_path = temp_file),
    class = "interrupt"
  )
  
  chat_resume <- chat_batch()
  chat_resume$register_tool(get_square_tool())
  result <- chat_resume$batch(prompts, state_path = temp_file)
  
  expect_equal(length(result$texts()), 3)
})