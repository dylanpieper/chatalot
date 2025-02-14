test_that("chat_batch initialization works", {
  chat <- chat_batch()
  expect_s3_class(chat, "ellmer_chat")
  expect_type(chat$batch, "closure")
})

test_that("chat_batch processes prompts correctly", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch()
  prompts <- list("What is 2+2?", "Name a planet.")
  
  result <- chat$batch(prompts)
  
  expect_type(result, "list")
  expect_s3_class(result, "batch")
  expect_equal(length(result$texts()), 2)
  expect_true(all(sapply(result$chats(), function(x) inherits(x, "ellmer_chat"))))
})

test_that("chat_batch handles errors gracefully", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_batch()
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "invalid_key")
  
  expect_error(
    chat$batch(list("Test prompt")),
    regexp = "auth|unauthorized|invalid.*key",
    ignore.case = TRUE
  )
  
  Sys.setenv(ANTHROPIC_API_KEY = old_key)
})