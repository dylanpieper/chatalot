# hellmer <img src="man/figures/hellmer-hex.png" align="right" width="140"/>

[![CRAN status](https://www.r-pkg.org/badges/version/hellmer)](https://CRAN.R-project.org/package=hellmer) [![R-CMD-check](https://github.com/dylanpieper/hellmer/actions/workflows/testthat.yml/badge.svg)](https://github.com/dylanpieper/hellmer/actions/workflows/testthat.yml)

hellmer makes it easy to batch process large language model chats using [ellmer](https://ellmer.tidyverse.org). Process many chats sequentially or in parallel and use ellmer features such as [tooling](https://ellmer.tidyverse.org/articles/tool-calling.html) and [structured data extraction](https://ellmer.tidyverse.org/articles/structured-data.html).

✅ hellmer processes many chats synchronously and supports streaming responses

❌ hellmer does NOT support asynchronous batch APIs (for example, see [OpenAI's Batch API](https://platform.openai.com/docs/guides/batch))

## Installation

You can install the package from CRAN with:

``` r
install.packages("hellmer")
```

## Setup API Keys

API keys allow access to chat models and are stored as environmental variables. I recommend the `usethis` package to setup API keys in your `.Renviron` such as `OPENAI_API_KEY=your-key`.

``` r
usethis::edit_r_environ(scope = c("user", "project"))
```

## Basic Usage

For the following examples, define a chat object to reuse across batches.

``` r
openai <- chat_openai(system_prompt = "Reply concisely, one sentence")
```

### Sequential Processing

Sequential processing uses the current R process to call one chat at a time and save the data to the disk.

``` r
library(hellmer)

chat <- chat_sequential(openai)

prompts <- list(
  "What is R?",
  "Explain base R versus tidyverse"
)

batch <- chat$batch(prompts)
```

Access the batch results:

``` r
batch$progress()
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

batch$texts()
#> [[1]]
#> [1] "R is a programming language and software environment primarily used for 
#> statistical computing and data analysis."
#> 
#> [[2]]
#> [1] "Base R refers to the R language's core packages and functionalities, 
#> whereas Tidyverse is a collection of R packages designed for data science 
#> that provides a more intuitive and consistent syntax."

batch$chats()
#> [[1]]
#> <Chat OpenAI/gpt-4o turns=3 tokens=22/18>
#> ── system [0] ───────────────────────────────────────────────────────────────
#> Reply concisely, one sentence
#> ── user [22] ────────────────────────────────────────────────────────────────
#> What is R?
#> ── assistant [18] ───────────────────────────────────────────────────────────
#> R is a programming language and software environment primarily used for
#> statistical computing and data analysis.

#> [[2]]
#> <Chat OpenAI/gpt-4o turns=3 tokens=24/37>
#> ── system [0] ───────────────────────────────────────────────────────────────
#> Reply concisely, one sentence
#> ── user [24] ────────────────────────────────────────────────────────────────
#> Explain base R versus tidyverse
#> ── assistant [37] ───────────────────────────────────────────────────────────
#> Base R refers to the R language's core packages and functionalities, whereas 
#> Tidyverse is a collection of R packages designed for data science 
#> that provides a more intuitive and consistent syntax.
```

### Parallel Processing

Parallel processing spins up multiple R processes, or parallel workers, to chat at the same time.

By default, the upper limit for number of `workers` = `parallel::detectCores()`, and the number of prompts to process at a time is `chunk_size` = `parallel::detectCores() * 5`. Each chat in a chunk is distributed across the available R processes. When a chunk is finished, the data is saved to the disk.

``` r
chat <- chat_future(openai)
```

For maximum performance, set `chunk_size` to the number of prompts (\~4-5x faster). However, data will not be saved to the disk until all chats are processed.

``` r
batch <- chat$batch(
  prompts, 
  chunk_size = length(prompts)
)
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

batch <- chat$batch(prompts)

batch$texts()
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

batch <- chat$batch(prompts, type_spec = type_sentiment)

batch$texts()
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

#### Self-evaluation

Self-evaluation prompts the chat model to improve the initial or prior structured data extraction using the `eval_rounds` parameter (increases token use). You can set the number of self-evaluation rounds but be mindful of the cost and risk of diminishing returns.

``` r
batch <- chat$batch(prompts, type_spec = type_sentiment, eval_rounds = 1)

batch$texts()
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

### Progress Tracking and Recovery

Batch processing state and progress is saved to a path to an `.rds` file on the disk and allows you to resume interrupted operations:

``` r
batch <- chat$batch(prompts, state_path = "chat_state.rds")
batch$progress()
```

If `state_path` is not defined, a temporary file will be created by default.

### Automatic Retry

Automatically retry failed requests with exponential backoff, which is useful to allow batch processing to persist for transient errors such as exceeding rate limits and temporary server errors.

Most chat provider functions in `ellmer` do retry at least one time by default, but there is no user-defined control over the retry strategy.

``` r
batch <- chat$batch(
  prompts = prompts,   # list or vector of prompts
  max_retries = 3,     # maximum retry attempts
  initial_delay = 20,  # initial delay in seconds
  max_delay = 80,      # maximum delay between retries
  backoff_factor = 2   # multiply delay by this factor after each retry
)
```

### Sound Notifications

Toggle sound notifications on batch completion, interruption, and error:

``` r
batch <- chat$batch(prompts, beep = TRUE)
```

### Echoing

By default, the chat `echo` is set to `FALSE` to show a progress bar. However, you can still configure `echo` by first setting `progress` to `FALSE`:

``` r
batch <- chat$batch(prompts, progress = FALSE, echo = "all")
#> > What is R?
#> < R is a programming language and software environment used for statistical computing,
#> < data analysis, and graphical representation.
#> < 
#> > Explain base R versus tidyverse
#> < Base R refers to the functions and paradigms built into the R language, while
#> < tidyverse is a collection of R packages designed for data science, emphasizing 
#> < a more consistent and human-readable syntax for data manipulation.
#> < 
```

### Methods

-   `progress()`: Returns processing status
-   `texts()`: Returns response texts in the same format as the input prompts (i.e., a list if prompts were provided as a list, or a character vector if prompts were provided as a vector). When a type specification is provided, it returns structured data instead of plain text.
-   `chats()`: Returns a list of chat objects

## Further Reading

-   [Using Ellmer Chat Models](https://dylanpieper.github.io/hellmer/articles/using-chat-models.html) (Vignette)
-   [Batch and Compare the Similarity of LLM Responses in R](https://dylanpieper.github.io/blog/posts/batch-and-compare-LLM-responses.html) (Blog Post)
