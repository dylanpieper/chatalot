test_that("capture_with_retry handles different error types correctly", {
  create_timeout_error <- function() {
    structure(
      list(message = "Request Timeout"),
      class = c("httr2_http_429", "httr2_http", "error", "condition")
    )
  }

  create_unauthorized_error <- function() {
    structure(
      list(message = "Unauthorized"),
      class = c("httr2_http_401", "httr2_http", "error", "condition")
    )
  }

  create_retry_chat <- function(max_fails = 2, error_type = "timeout", succeed = TRUE) {
    shared_counter <- new.env(parent = emptyenv())
    shared_counter$attempts <- 0

    make_chat <- function() {
      structure(
        list(
          clone = function() {
            make_chat()
          },
          chat = function(prompt, echo = FALSE, ...) {
            shared_counter$attempts <- shared_counter$attempts + 1

            if (error_type == "timeout" && shared_counter$attempts <= max_fails) {
              cli::cli_alert_info("Mock: attempt {shared_counter$attempts}/{max_fails} (timeout)")
              stop(create_timeout_error())
            } else if (error_type == "unauthorized") {
              cli::cli_alert_info("Mock: unauthorized error")
              stop(create_unauthorized_error())
            } else if (succeed) {
              cli::cli_alert_success("Mock: succeeded on attempt {shared_counter$attempts}")
              "Success after retry!"
            } else {
              cli::cli_alert_info("Mock: continuing to fail")
              stop(create_timeout_error())
            }
          },
          extract_data = function(prompt, type, echo = FALSE, ...) {
            shared_counter$attempts <- shared_counter$attempts + 1

            if (error_type == "timeout" && shared_counter$attempts <= max_fails) {
              cli::cli_alert_info("Mock: attempt {shared_counter$attempts}/{max_fails} (timeout)")
              stop(create_timeout_error())
            } else if (error_type == "unauthorized") {
              cli::cli_alert_info("Mock: unauthorized error")
              stop(create_unauthorized_error())
            } else if (succeed) {
              cli::cli_alert_success("Mock: succeeded on attempt {shared_counter$attempts}")
              list(status = "Success after retry!")
            } else {
              cli::cli_alert_info("Mock: continuing to fail")
              stop(create_timeout_error())
            }
          },
          get_attempt_count = function() {
            shared_counter$attempts
          }
        ),
        class = c("mock_chat", "R6")
      )
    }

    make_chat()
  }

  mock_chat <- create_retry_chat(max_fails = 2, error_type = "timeout", succeed = TRUE)
  result <- hellmer:::capture_with_retry(
    original_chat = mock_chat,
    prompt = "test prompt",
    type_spec = NULL,
    judgements = 0,
    max_retries = 3,
    initial_delay = 0.01,
    max_delay = 0.03,
    backoff_factor = 1.5,
    echo = FALSE
  )

  expect_true(!is.null(result))

  mock_chat <- create_retry_chat(max_fails = 4, error_type = "timeout", succeed = FALSE)
  expect_error(
    hellmer:::capture_with_retry(
      original_chat = mock_chat,
      prompt = "test prompt",
      type_spec = NULL,
      judgements = 0,
      max_retries = 3,
      initial_delay = 0.01,
      max_delay = 0.03,
      backoff_factor = 1.5,
      echo = FALSE
    ),
    "Failed after 3 attempts"
  )

  mock_chat <- create_retry_chat(error_type = "unauthorized")

  expect_error(
    hellmer:::capture_with_retry(
      original_chat = mock_chat,
      prompt = "test prompt",
      type_spec = NULL,
      judgements = 0,
      max_retries = 3,
      initial_delay = 0.01,
      max_delay = 0.03,
      backoff_factor = 1.5,
      echo = FALSE
    )
  )

  expect_equal(mock_chat$get_attempt_count(), 1)
})
