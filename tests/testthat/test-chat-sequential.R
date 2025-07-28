test_that("seq_chat initialization and result class works", {
  skip_chat_test()
  chat <- create_test_chat(seq_chat)
  expect_true(inherits(chat, "Chat"))
  expect_true(inherits(chat, "R6"))
  result <- chat$process(get_test_prompts(1), beep = FALSE)
  expect_true(inherits(result$chats()[[1]], c("Chat", "R6")))
})

test_that("seq_chat processes prompts correctly", {
  skip_chat_test()
  chat <- create_test_chat(seq_chat)
  result <- chat$process(get_test_prompts(2), beep = FALSE)
  validate_process_result(result, 2)
})

test_that("seq_chat processes content prompts correctly", {
  skip_chat_test()
  chat <- create_test_chat(seq_chat)
  result <- chat$process(get_content_prompts(), beep = FALSE)
  validate_process_result(result, 2)
})

test_that("seq_chat handles structured data extraction", {
  skip_chat_test()
  chat <- create_test_chat(seq_chat)
  result <- chat$process(get_sentiment_prompts(), type = get_sentiment_type_spec(), beep = FALSE)
  data <- result$texts()
  validate_structured_data(data, 2)
})


test_that("seq_chat works with tools", {
  skip_chat_test()
  chat <- create_test_chat(seq_chat)
  chat$register_tool(get_square_tool())
  result <- chat$process(
    get_tool_prompts(),
    beep = FALSE
  )
  validate_process_result(result, 2)
})

test_that("seq_chat handles state persistence", {
  skip_chat_test()
  chat <- create_test_chat(seq_chat)
  create_temp_file_test(chat)
})

test_that("seq_chat supports progress parameter", {
  skip_chat_test()
  test_progress_parameter(seq_chat)
})

test_that("seq_chat doesn't break with echo parameter", {
  skip_chat_test()
  test_echo_parameter(seq_chat)
})

test_that("seq_chat handles errors gracefully", {
  skip_chat_test()
  test_api_key_failure(seq_chat)
})
