test_that("chat_parallel initialization works", {
  chat <- chat_parallel()
  expect_s3_class(chat, "ellmer_chat")
  expect_type(chat$batch, "closure")
})

test_that("chat_parallel processes chunks correctly", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel(workers = 2)
  prompts <- list(
    "What is 2+2?",
    "Name a planet.",
    "What color is the sky?",
    "Count to 3."
  )
  
  result <- chat$batch(prompts, chunk_size = 2)
  
  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 4)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, "ellmer_chat"))))
})

test_that("chat_parallel handles worker failures", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel(workers = 2)
  prompts <- list("What is 2+2?", "Name a planet.")
  
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "invalid_key")
  
  expect_error(
    chat$batch(prompts, chunk_size = 1),
    regexp = "auth|unauthorized|invalid.*key",
    ignore.case = TRUE
  )
  
  Sys.setenv(ANTHROPIC_API_KEY = old_key)
})