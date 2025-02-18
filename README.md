# hellmer <img src="man/figures/hellmer-hex.png" align="right" width="140"/>

This package enables sequential or parallel batch processing for [chat models](https://ellmer.tidyverse.org/reference/index.html) from [ellmer](https://github.com/tidyverse/ellmer).

## Overview

Process multiple chat interactions with:

-   Ellmer's [tooling](https://ellmer.tidyverse.org/articles/tool-calling.html) and [structured data extraction](https://ellmer.tidyverse.org/articles/structured-data.html)
-   State persistence and recovery
-   Progress tracking
-   Configurable output verbosity
-   Automatic retry with backoff
-   Timeout handling
-   Sound notifications

## Installation

``` r
devtools::install_github("dylanpieper/hellmer")
```

## Load Package

Run `library(hellmer)` to get started. This package attaches `ellmer` for easy access to the chat models.

## Basic Usage

### Sequential Processing

``` r
chat <- chat_sequential(chat_claude, system_prompt = "Reply concisely")

prompts <- list(
  "What is 2+2?",
  "Name one planet.",
  "Is water wet?",
  "What color is the sky?",
  "Count to 3.",
  "Say hello.",
  "Name a primary color.",
  "What is 5x5?",
  "True or false: Birds can fly.",
  "What day comes after Monday?"
)

result <- chat$batch(prompts)

result$progress()
result$texts()
result$chats()
```

### Parallel Processing

Simply swap `chat_sequential()` for `chat_future()` to enable parallel processing.

``` r
chat <- chat_future(chat_claude, system_prompt = "Reply concisely")
```

## Features

### Tooling

Register and use tools/function calling:

``` r
square_number <- function(num) num^2

chat$register_tool(tool(
  square_number,
  "Calculates the square of a given number",
  num = type_integer("The number to square")
))

prompts <- list(
  "What is the square of 3?",
  "Calculate the square of 5."
)
```

### Structured Data Extraction

Extract structured data using type specifications:

``` r
type_sentiment <- type_object(
  "Extract sentiment scores",
  positive_score = type_number("Positive sentiment score, 0.0 to 1.0"),
  negative_score = type_number("Negative sentiment score, 0.0 to 1.0"),
  neutral_score = type_number("Neutral sentiment score, 0.0 to 1.0")
)

prompts <- list(
  "I love this product! It's amazing!",
  "This is okay, nothing special.",
  "Terrible experience, very disappointed."
)

result <- chat$batch(prompts, type_spec = type_sentiment)
structured_data <- result$structured_data()
```

### State Management

Batch processing automatically saves state and can resume interrupted operations:

``` r
result <- chat$batch(prompts, state_path = "chat_state.rds")
```

If `state_path` is not defined, a temporary file will be created by default.

### Output Control

Control verbosity with the `echo` parameter (sequential only):

-   `"none"`: Silent operation with progress bar
-   `"text"`: Show chat responses only
-   `"all"`: Show both prompts and responses

``` r
chat <- chat_sequential(
  chat_claude, 
  echo = "none"
)
```

### Automatic Retry

Automatically retry failed requests with backoff, which serves as a wide guardrail against token/RPM limits and random errors:

``` r
chat <- chat_sequential(
  chat_claude,         # Base chat model
  max_retries = 3,     # Maximum number of retry attempts
  initial_delay = 20,  # Initial delay in seconds
  max_delay = 60,      # Maximum delay between retries
  backoff_factor = 2   # Multiply delay by this factor after each retry
)
```

If a request fails, the code will:

1.  Wait for the `initial_delay`
2.  Retry the request
3.  If it fails again, wait for (delay × `backoff_factor`)
4.  Continue until success or `max_retries` is reached

If the code detects an authorization or API key issue, it will stop immediately.

### Timeout Handling

The timeout parameter specifies the maximum time to wait for a response from the chat model for each prompt. However, this parameter is still limited by the timeouts propagated up from the chat models.

``` r
chat <- chat_future(
  chat_ollama,
  model = "deepseek-r1:8b",
  system_prompt = "Reply in one sentence or less",
  timeout = 60
)
```

### Sound Notifications

Toggle sound notifications on batch completion, interruption, and error:

``` r
chat <- chat_sequential(
  chat_claude,
  beep = TRUE
)
```

## Quick References

### chat_sequential()

Creates a sequential batch processor.

``` r
chat_sequential(
  chat_model = chat_claude,  # Ellmer chat model
  echo = "none",             # Output verbosity (sequential only)
  beep = TRUE,               # Toggle sound notifications
  max_retries = 3L,          # Maximum retry attempts
  initial_delay = 20,        # Initial retry delay in seconds
  max_delay = 60,            # Maximum delay between retries
  backoff_factor = 2,        # Retry backoff multiplier
  timeout = 60,              # Maximum seconds to wait for response
  ...                        # Pass parameters to the chat model
)
```

### chat_future()

Creates a parallel batch processor.

``` r
chat_future(
  chat_model = chat_claude,  # Ellmer chat model
  workers = 4L,              # Number of parallel workers
  plan = "multisession",     # Options: "multisession" or "multicore"
  beep = TRUE,               # Enable sound notifications
  chunk_size = 4L,           # Number of prompts to process in parallel at a time 
  max_chunk_attempts = 3L,   # Maximum retries for failed chunks
  max_retries = 3,           # Maximum retry attempts
  initial_delay = 20,        # Initial retry delay in seconds
  max_delay = 60,            # Maximum delay between retries
  backoff_factor = 2,        # Retry backoff multiplier
  timeout = 60,              # Maximum seconds to wait for response
  ...                        # Pass parameters to the chat model
)
```

### batch\$batch()

Processes a list or vector of prompts.

``` r
batch(
  prompts,                                  # List of prompts to process
  type_spec = NULL,                         # Type specification for structured data
  state_path = tempfile("chat_",            # Path for state persistence
                        fileext = ".rds"),
  chunk_size = 4                            # Number of prompts per chunk (parallel only)
)
```

You can mimic sequential processing when using `chat_future()` by setting the `chunk_size = 1`, but this will likely decrease performance compared to `chat_sequential()` (see `tests/manual/test-benchmark.R`).

### Results Methods

-   `texts()`: Returns response texts in the same format as the input prompts (i.e., a list if prompts were provided as a list, or a character vector if prompts were provided as a vector)
-   `chats()`: Returns a list of chat objects
-   `progress()`: Returns processing statistics
-   `structured_data()`: Returns extracted structured data (if `type_spec` is provided)

## Further Reading

-   [Using Ellmer Chat Models](https://dylanpieper.github.io/hellmer/articles/using-chat-models.html): Are you wondering if you can use `chat_claude()` as a user-defined object instead of the `chat_claude` function? Of course you can! Learn more the two methods and the default interface.
-   [Comparing Packages for Batching LLM Tasks](https://dylanpieper.github.io/hellmer/articles/comparing-packages.html)
