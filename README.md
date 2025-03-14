# hellmer <img src="man/figures/hellmer-hex.png" align="right" width="140"/>

`hellmer` enables sequential or parallel batch processing for [chat models](https://ellmer.tidyverse.org/reference/index.html) supported by `ellmer`.

## Overview

Process multiple chat interactions with:

-   [Tooling](https://ellmer.tidyverse.org/articles/tool-calling.html) and [structured data extraction](https://ellmer.tidyverse.org/articles/structured-data.html)
-   State persistence and recovery
-   Progress tracking
-   Configurable output verbosity
-   Automatic retry with backoff
-   Timeout handling
-   Sound notifications

## Installation

You can install the package from CRAN with:

``` r
install.packages("hellmer")
```

## Setup API Keys

`ellmer` will look for API keys in your environmental variables. I recommend the `usethis` package to setup API keys in your `.Renviron` such as `OPENAI_API_KEY=your-key`.

``` r
usethis::edit_r_environ(scope = c("user", "project"))
```

## Basic Usage

### Sequential Processing

``` r
library(hellmer)

chat <- chat_sequential(
  chat_openai, 
  system_prompt = "Reply concisely, one sentence"
)

prompts <- list(
  "What is R?",
  "Explain base R versus tidyverse"
)

result <- chat$batch(prompts)
```

Access the results:

``` r
result$progress()
#> $total_prompts
#> [1] 2
#> 
#> $completed_prompts
#> [1] 2
#> 
#> $completion_percentage
#> [1] 100
#> 
#> $remaining_prompts
#> [1] 0
#> 
#> $state_path
#> [1] "/var/folders/.../chat_c5383b1279ae.rds"

result$texts()
#> [[1]]
#> [1] "R is a programming language and software environment used for 
#> statistical computing, data analysis, and graphical representation."
#> 
#> [[2]]
#> [1] "Base R refers to the original set of R functions and packages, 
#> while tidyverse is a collection of R packages designed for data science
#> that offer a more consistent and readable syntax."

result$chats()
#> [[1]]
#> <Chat turns=3 tokens=22/21>
#>  ── system ────────────────────────────────────────────────────────────
#> Reply concisely, one sentence
#> ── user ───────────────────────────────────────────────────────────────
#> What is R?
#> ── assistant ──────────────────────────────────────────────────────────
#> R is a programming language and software environment used for 
#> statistical computing, data analysis, and graphical representation.
#> ...
```

### Parallel Processing

``` r
chat <- chat_future(chat_openai, 
                    system_prompt = "Reply concisely, one sentence")
```

#### Performance vs Safety Trade-Off

When using parallel processing with `chat_future`, there's a trade-off between safety and performance:

-   **Maximum Safety**: Using a smaller `chunk_size` ensures progress is saved to the disk more frequently, allowing recovery if something goes wrong (default: number of prompts / 10)
-   **Maximum Performance**: Setting `chunk_size` equal to the number of prompts results in a 4-5x faster processing speed but progress will not be saved to the disk until all chats are processed

``` r
chat$batch(prompts, chunk_size = length(prompts))
```

## Features

### Tooling

Register and use tools/function calling:

``` r
get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}

chat$register_tool(tool(
  get_current_time,
  "Gets the current time in the given time zone.",
  tz = type_string(
    "The time zone to get the current time in. Defaults to `\"UTC\"`.",
    required = FALSE
  )
))

prompts <- list(
  "What time is it in Chicago?",
  "What time is it in New York?"
)

result <- chat$batch(prompts)

result$texts()
#> [[1]]
#> [1] "The current time in Chicago is 9:29 AM CDT."
#> 
#> [[2]]
#> [1] "The current time in New York is 10:29 AM EDT."
```

### Structured Data Extraction

Extract structured data using type specifications:

``` r
type_sentiment <- type_object(
  "Extract sentiment scores",
  positive_score = type_number("Positive sentiment score, 0.00 to 1.00"),
  negative_score = type_number("Negative sentiment score, 0.00 to 1.00"),
  neutral_score = type_number("Neutral sentiment score, 0.00 to 1.00")
)

prompts <- list(
  "The R community is really supportive and welcoming.",
  "R has both base functions and tidyverse functions for data manipulation.",
  "R's object-oriented system is confusing, inconsistent, and painful to use."
)

result <- chat$batch(prompts, type_spec = type_sentiment)

result$texts()
#> [[1]]
#> $positive_score
#> [1] 0.95
#> 
#> $negative_score
#> [1] 0.05
#> 
#> $neutral_score
#> [1] 0
#> ...
```

To iteratively refine structured data extractions, implement LLM-as-a-judge into the chat turns using the `judgements` parameter (increases token use):

``` r
result <- chat$batch(prompts, type_spec = type_sentiment, judgements = 1)

result$texts()
#> [[1]]
#> [[1]]$positive_score
#> [1] 0.95
#> 
#> [[1]]$negative_score
#> [1] 0
#> 
#> [[1]]$neutral_score
#> [1] 0.05
#> ...
```

### State Management

Batch processing automatically saves progress to an `.rds` file on the disk and allows you to resume interrupted operations:

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
  chat_openai, 
  echo = "none"
)
```

### Automatic Retry

Automatically retry failed requests with exponential backoff, which serves as a wide guardrail against errors while `ellmer` and `httr2` serve as a narrow guardrail against specific API limits:

``` r
chat <- chat_sequential(
  chat_openai,         # ellmer chat model
  max_retries = 3,     # maximum retry attempts
  initial_delay = 20,  # initial delay in seconds
  max_delay = 80,      # maximum delay between retries
  backoff_factor = 2   # multiply delay by this factor after each retry
)
```

If a request fails, the code will:

1.  Wait for the `initial_delay`
2.  Retry the request
3.  If it fails again, wait for (delay × `backoff_factor`)
4.  Continue until success or `max_retries` is reached

If the code detects an authorization or API key issue, it will stop immediately.

### Timeout Handling

The timeout parameter specifies the maximum time to wait for a response from the chat model for each prompt. However, this parameter is still limited by the timeouts propagated up from the chat model functions.

``` r
chat <- chat_future(
  chat_openai,
  system_prompt = "Reply concisely, one sentence"
  timeout = 60
)
```

### Sound Notifications

Toggle sound notifications on batch completion, interruption, and error:

``` r
chat <- chat_sequential(
  chat_openai,
  beep = TRUE
)
```

### Results Methods

-   `progress()`: Returns processing status
-   `texts()`: Returns response texts in the same format as the input prompts (i.e., a list if prompts were provided as a list, or a character vector if prompts were provided as a vector). When a type specification is provided, it returns structured data instead of plain text.
-   `chats()`: Returns a list of chat objects

## Further Reading

-   [Using Ellmer Chat Models](https://dylanpieper.github.io/hellmer/articles/using-chat-models.html): Are you wondering if you can use `chat_openai()` as a user-defined object instead of the `chat_openai` function? Of course you can! Learn more about the two methods and the default interface.
