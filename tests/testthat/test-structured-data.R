test_that("structured data extraction works", {
  skip_if_not(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "API key not available")
  
  chat <- chat_parallel()
  type_spec <- type_object(
    "Extract sentiment",
    score = type_number("Sentiment score from -1 to 1")
  )
  
  prompts <- list(
    "I love this!",
    "This is terrible."
  )
  
  result <- chat$batch(prompts, type_spec = type_spec)
  data <- result$structured_data()
  
  expect_type(data, "list")
  expect_length(data, 2)
  expect_true(all(sapply(data, function(x) !is.null(x$score))))
  expect_true(all(sapply(data, function(x) x$score >= -1 && x$score <= 1)))
})