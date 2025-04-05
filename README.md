# hellmer <img src="man/figures/hellmer-hex.png" align="right" width="140"/>

[![CRAN status](https://www.r-pkg.org/badges/version/hellmer)](https://cran.r-pkg.org/package=hellmer) [![R-CMD-check](https://github.com/dylanpieper/hellmer/actions/workflows/testthat.yml/badge.svg)](https://github.com/dylanpieper/hellmer/actions/workflows/testthat.yml)

Enable sequential and parallel batch processing for [chat models](https://ellmer.tidyverse.org/reference/index.html#chatbots) supported by `ellmer`.

## Features

Process multiple chat interactions with:

-   [Tooling](https://ellmer.tidyverse.org/articles/tool-calling.html) and [structured data extraction](https://ellmer.tidyverse.org/articles/structured-data.html)
-   LLM-as-a-judge for structured data refinement
-   Progress tracking and recovery
-   Automatic retry with backoff
-   Sound notifications

## Installation

You can install the package from CRAN with:

``` r
install.packages("hellmer")
```

## Setup API Keys

API keys allow access to chat models are are stored as environmental variables. I recommend the `usethis` package to setup API keys in your `.Renviron` such as `OPENAI_API_KEY=your-key`.

``` r
usethis::edit_r_environ(scope = c("user", "project"))
```

## Basic Usage

### Sequential Processing

``` r
library(hellmer)

chat <- chat_sequential(chat_openai(system_prompt = "Reply concisely, one sentence"))

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
#> [1] "R is a programming language and software environment primarily used for 
#> statistical computing and data analysis."
#> 
#> [[2]]
#> [1] "Base R refers to the R language's core packages and functionalities, 
#> whereas Tidyverse is a collection of R packages designed for data science 
#> that provides a more intuitive and consistent syntax."

result$chats()
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

Parallel processing spins up multiple R processes to run chats at the same time.

``` r
chat <- chat_future(chat_openai(system_prompt = "Reply concisely, one sentence"))
```

When using parallel processing with `chat_future`, there's a trade-off between safety and performance:

-   **Maximum Safety**: Using a smaller `chunk_size` ensures progress is saved to the disk more frequently, allowing recovery if something goes wrong (default: CPU cores \* 5)
-   **Maximum Performance**: Setting `chunk_size` equal to the number of prompts results in a 4-5x faster processing speed but progress will not be saved to the disk until all chats are processed

``` r
chat$batch(
  prompts, 
  chunk_size = length(prompts)
)
```

Because `workers` = CPU cores is a heuristic for how many R Sessions to spin up, you can improve performance by multiplying the CPU cores to any factor:

``` r
chat$batch(
  prompts, 
  chunk_size = length(prompts), 
  workers = parallel::detectCores() * 5
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

To ask the chat model to evaluate and refine structured data extractions, implement LLM-as-a-judge into the turns of the chat using the `judgements` parameter (increases token use):

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

![Console output of LLM streaming the evaluation and refinement of the structured data extractions using `progress` = `FALSE` and `echo` = `TRUE`.](man/figures/judgements.gif)

### Progress Tracking and Recovery

Batch processing automatically saves progress to an `.rds` file on the disk and allows you to resume interrupted operations:

``` r
result <- chat$batch(prompts, state_path = "chat_state.rds")
result$progress()
```

If `state_path` is not defined, a temporary file will be created by default.

### Automatic Retry

Automatically retry failed requests with exponential backoff, which acts as a wide guardrail against temporary API errors. `ellmer` uses `httr2` to act as a narrow guardrail against specific API errors and limits with most chat provider functions defaulting to retry one time.

Be aware that this retry is a brute force approach, and as long as all other validation passes, the retry will persist. However, it will stop if it detects an authorization or API key issue.

``` r
result <- chat$batch(
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
chat <- chat_sequential(
  chat_openai,
  beep = TRUE
)
```

### Methods

-   `progress()`: Returns processing status
-   `texts()`: Returns response texts in the same format as the input prompts (i.e., a list if prompts were provided as a list, or a character vector if prompts were provided as a vector). When a type specification is provided, it returns structured data instead of plain text.
-   `chats()`: Returns a list of chat objects

### Echoing

By default, the chat `echo` is set to `FALSE` to show a progress bar. However, you can still configure `echo` in the `$batch` call by first setting `progress` to `FALSE`:

``` r
result <- chat$batch(prompts, progress = FALSE, echo = "all")
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

## Further Reading

-   [Using Ellmer Chat Models](https://dylanpieper.github.io/hellmer/articles/using-chat-models.html)
