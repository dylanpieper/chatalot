# hellmer <img src="man/figures/hellmer-hex.png" align="right" height="140"/>

This package implements batch processing for chat models from the [ellmer](https://github.com/tidyverse/ellmer) package. It uses sequential processing that serves as an interim solution until parallel processing is integrated into ellmer using [httr2](https://httr2.r-lib.org) which will significantly boost performance ([#143](https://github.com/tidyverse/ellmer/issues/143)).

## Installation

``` r
devtools::install_github("dylanpieper/hellmer")
```

## Overview

Process multiple chat interactions in sequence with:

-   State persistence and recovery
-   Progress tracking
-   Structured data extraction
-   Tool integration
-   Configurable output verbosity
-   Automatic retry with exponential backoff
-   Sound notifications

## Basic Usage

``` r
library(hellmer)

chat <- chat_batch(chat_claude("You reply concisely"))

prompts <- list(
  "What is 2+2?",
  "Name one planet.",
  "Is water wet?"
)

result <- chat$batch(prompts)

result$progress()

result$texts()
result$chats()
```

## Features

### State Management

Batch processing automatically saves state and can resume interrupted operations:

``` r
result <- chat$batch(prompts, state_path = "chat_state.rds")
```

If `state_path` is not defined, a temp file will be created by default.

### Structured Data Extraction

Extract structured data using type specifications:

``` r
type_sentiment <- type_object(
  "Extract sentiment scores",
  positive_score = type_number("Positive sentiment score, 0.0 to 1.0"),
  negative_score = type_number("Negative sentiment score, 0.0 to 1.0"),
  neutral_score = type_number("Neutral sentiment score, 0.0 to 1.0")
)

result <- chat$batch(prompts, type_spec = type_sentiment)
structured_data <- result$structured_data()
```

### Tool Integration

Register and use tools in batch processing:

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

### Output Control

Control verbosity with the `echo` parameter:

-   `"none"`: Silent operation with progress bar
-   `"text"`: Show chat responses only
-   `"all"`: Show both prompts and responses

``` r
chat <- chat_batch(
  chat_claude("You reply concisely"), 
  echo = "none"
)
```

### Automatic Retry

The system automatically retries failed requests with exponential backoff:

``` r
chat <- chat_batch(
  chat_claude(),
  max_retries = 3,        # Maximum number of retry attempts
  initial_delay = 1,      # Initial delay in seconds
  max_delay = 32,         # Maximum delay between retries
  backoff_factor = 2      # Multiply delay by this factor after each retry
)
```

If a request fails, the system will: 
1. Wait for the `initial_delay`
2. Retry the request 
3. If it fails again, wait for (delay × `backoff_factor`) 
4. Continue until success or `max_retries` is reached

⚠️ The retry is not smart and will retry for any error including an invalid API key.

### Sound Notifications

Toggle sound notifications on batch completion, interruption, and error:

``` r
chat <- chat_batch(
  chat_claude(),
  beep = FALSE
)
```

## References

### chat_batch()

Creates a batch-enabled chat instance.

``` r
chat_batch(
  chat_model = chat_openai(),  # Base chat model
  echo = "text",               # Output verbosity
  beep = TRUE,                 # Enable sound notifications
  max_retries = 3,             # Maximum retry attempts
  initial_delay = 1,           # Initial retry delay in seconds
  max_delay = 32,              # Maximum delay between retries
  backoff_factor = 2           # Exponential backoff multiplier
)
```

### batch\$batch()

Processes a list of prompts.

``` r
batch(
  prompts,                # List of prompts to process
  type_spec = NULL,       # Optional type specification for structured data
  state_path = NULL       # Optional path for state persistence
)
```

### Results Methods

-   `texts()`: Returns response texts in the same format as the input prompts (i.e., a character vector if prompts were provided as a vector, or a list if prompts were provided as a list)
-   `chats()`: Returns a list of chat objects
-   `progress()`: Returns processing statistics
-   `structured_data()`: Returns extracted structured data (if `type_spec` is provided)

### Todo

-   ✅ Add retries on error
-   ✅ Change `texts()` to return a list or vector matching the input
-   ✅ Add sound notifications on completion, interruption, and error
