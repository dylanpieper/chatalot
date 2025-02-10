# hellmer <img src="man/figures/hellmer.png" align="right" height="140"/>

**Take the helm of [ellmer](https://github.com/tidyverse/ellmer)!**

This package provides batch processing capabilities for chat models from the ellmer package. It implements safe state handling and batch operations using S7 classes. This is a sequential processing implementation that serves as an interim solution until parallel processing is integrated into ellmer using [httr2](https://httr2.r-lib.org) which will significantly boost performance.

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

## Basic Usage

``` r
library(hellmer)

# Initialize a batch-enabled chat
chat <- chat_batch(
  chat_claude("You reply concisely"), 
  echo = "none"  # Options: "none", "text", "all"
)

# Define prompts
prompts <- list(
  "What is 2+2?",
  "Name one planet.",
  "Is water wet?"
)

# Process batch
result <- chat$batch(prompts)

# Get text responses
result$texts()

# Get full chat objects
result$chats()

# Check progress
result$progress()
```

## Features

### State Management

Batch processing automatically saves state and can resume interrupted operations:

``` r
# Specify a state file
result <- chat$batch(prompts, state_path = "chat_state.rds")
```

### Structured Data Extraction

Extract structured data using type specifications:

``` r
# Define data structure
type_sentiment <- type_object(
  "Extract sentiment scores",
  positive_score = type_number("Positive sentiment score, 0.0 to 1.0"),
  negative_score = type_number("Negative sentiment score, 0.0 to 1.0"),
  neutral_score = type_number("Neutral sentiment score, 0.0 to 1.0")
)

# Process with type specification
result <- chat$batch(prompts, type_spec = type_sentiment)

# Get structured data
structured_data <- result$structured_data()
```

### Tool Integration

Register and use tools in batch processing:

``` r
# Define and register a tool
square_number <- function(num) num^2

chat$register_tool(tool(
  square_number,
  "Calculates the square of a given number",
  num = type_integer("The number to square")
))

# Use tool in prompts
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

## References

### chat_batch()

Creates a batch-enabled chat instance.

``` r
chat_batch(
  chat_model = chat_openai(),  # Base chat model
  echo = "text"                # Output verbosity
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

-   `texts()`: Returns vector of response texts
-   `chats()`: Returns list of chat objects
-   `progress()`: Returns processing statistics
-   `structured_data()`: Returns extracted structured data (if type_spec provided)

### Todo

-   Add auto-retry if the connection is closed unexpectedly
