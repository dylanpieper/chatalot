Sys.setenv(ANTHROPIC_API_KEY = Sys.getenv("ANTHROPIC_API_KEY"))

get_test_prompts <- function(n = 3) {
  prompts <- c(
    "What is 2+2?",
    "Name a planet.",
    "What color is the sky?"
  )[1:n]
  return(as.list(prompts))
}

get_sentiment_type_spec <- function() {
  type_object(
    "Extract sentiment",
    score = type_number("Sentiment score from -1 to 1")
  )
}

get_square_tool <- function() {
  square_number <- function(num) num^2
  tool(
    square_number,
    "Calculates the square of a number",
    num = type_integer("The number to square")
  )
}

simulate_interrupt <- function(after_n_calls = 1) {
  interrupt_env <- new.env(parent = emptyenv())
  interrupt_env$count <- 0
  
  function() {
    interrupt_env$count <- interrupt_env$count + 1
    if (interrupt_env$count == after_n_calls) {
      signalCondition(structure(
        list(message = "Simulated interrupt"),
        class = c("interrupt", "condition")
      ))
    }
  }
}

create_interruptible_chat <- function(chat_fn = chat_batch, ...) {
  interrupt_hook <- simulate_interrupt(...)
  wrapped_chat <- chat_fn()
  original_batch <- wrapped_chat$batch
  
  wrapped_chat$batch <- function(prompts, ...) {
    tryCatch({
      interrupt_hook()
      original_batch(prompts, ...)
    }, interrupt = function(e) {
      stop(e)
    })
  }
  
  wrapped_chat
}