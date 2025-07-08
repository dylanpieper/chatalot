# chatalot <img src="man/figures/chatalot-hex.png" align="right" width="140"/>

[![CRAN status](https://www.r-pkg.org/badges/version/hellmer)](https://CRAN.R-project.org/package=hellmer) [![R-CMD-check](https://github.com/dylanpieper/hellmer/actions/workflows/testthat.yml/badge.svg)](https://github.com/dylanpieper/hellmer/actions/workflows/testthat.yml)

chatalot synchronously processes a lot of large language model chats in R using [ellmer](https://ellmer.tidyverse.org).

Easily setup sequential and parallel processing workflows with features including [tool calling](https://ellmer.tidyverse.org/articles/tool-calling.html), [structured data extraction](https://ellmer.tidyverse.org/articles/structured-data.html), checkpoint and resume, sound notification, and more.

chatalot is similar to existing ellmer tools:

-   [ellmer::parallel_chat()](https://ellmer.tidyverse.org/reference/parallel_chat.html) - Synchronously processes lots of chats in parallel. This tool is simple and fast but has limited features with no option to save data at checkpoints or resume if interuppted.

-   [ellmer::batch_chat()](https://ellmer.tidyverse.org/reference/batch_chat.html) - Asynchronously batch processes lots of chats from select providers. This tool is about 50% cheaper if you wait up to 24 hours for a response.

## Installation

You can install the development or CRAN version of the package with:

``` r
# pak::pak("dylanpieper/chatalot")
pak::pak("chatalot")
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

Sequential processing processes one chat at a time and saves the data to the disk after receiving each response.

``` r
library(chatalot)

chat <- chat_sequential(openai)

prompts <- c(
  "How to have the best vacation in Portugal?",
  "When is the best time of year to visit Portugal?",
  "What foods to expect as a tourist in Portugal?",
  "Which words to know in Portugese as a tourist?"
)

response <- chat$process(prompts)
```

Access the responses:

``` r
response$texts()
#> [1] "Plan ahead to include a mix of historic cities, coastal escapes, 
#> local cuisine, and authentic cultural experiences while keeping time 
#> for spontaneous discoveries."
#>                                                   
#> [2] "The best time to visit Portugal is during the shoulder seasons of spring 
#> (March-May) and fall (September-October) when the weather is pleasant and 
#> there are fewer crowds."       
#>                                                     
#> [3] "As a tourist in Portugal, you can expect a rich variety of seafood 
#> (like cod and grilled fish), hearty grilled meats, savory stews, delectable 
#> pastries such as pastel de nata, and locally produced wines and cheeses." 
#>              
#> [4] "Essential words include \"olá\" for hello, \"por favor\" for please, 
#> \"obrigado/obrigada\" for thank you, \"sim\" and \"não\" for yes and no, 
#> \"desculpe\" to apologize, \"quanto?\" for asking price, 
#> and \"banheiro\" for bathroom."
```

### Parallel Processing

**⚠️ Parallel processing is unavailable in ellmer 0.2.1 due to redacting API keys from chat objects with no callback, but this issue is temporary and will be fixed in ellmer.**

**✅ Parallel processing will work if you install `pak::pak("ellmer@0.2.0")`, but be aware that this version of ellmer exposes API keys in chat objects.**

------------------------------------------------------------------------

Parallel processing uses [future](https://www.futureverse.org) to create multiple R processes (workers) to chat at the same time. This method improves speed of processing.

The default upper limit for number of `workers` is `parallel::detectCores()`. The default `chunk_size` is also `parallel::detectCores()` and defines the number of prompts to process at a time. Each chat in a chunk is distributed across the available R processes. After a chunk is finished, data is saved to the disk.

``` r
chat <- chat_future(openai)
```

For maximum processing speed, set `chunk_size` to the number of prompts. However, be aware that data will not be saved to the disk until all chats are processed, risking data loss and additional cost.

``` r
response <- chat$process(
  prompts, 
  chunk_size = length(prompts)
)
```

## Features

### Tooling

Register and use [tool/function calling](https://ellmer.tidyverse.org/articles/tool-calling.html):

``` r
weather <- data.frame(
  city = c("Chicago", "NYC", "Lisbon"),
  raining = c("heavy", "none", "overcast"),
  temperature = c("cool", "hot", "warm"),
  wind = c("strong", "weak", "strong")
)

get_weather <- function(cities) weather[weather$city %in% cities, ]

chat$register_tool(tool(
  get_weather,
  "Report on weather conditions.",
  cities = type_array("City names", type_string())
))

response <- chat$process(interpolate("Give me a weather update for {{weather$city}}?"))

response$texts()
#> [1] "In Chicago, it's cool with heavy rain and strong winds."                   
#> [2] "The current weather in NYC is hot with no rain and light winds."           
#> [3] "Lisbon currently has an overcast sky, warm temperatures, and strong winds."
```

### Structured Data Extraction

Extract [structured data](https://ellmer.tidyverse.org/articles/structured-data.html) using type specifications:

``` r
prompts <- c(
  "I go by Alex. 42 years on this planet and counting.",
  "Pleased to meet you! I'm Jamal, age 27.",
  "They call me Li Wei. Nineteen years young.",
  "Fatima here. Just celebrated my 35th birthday last week.",
  "The name's Robert - 51 years old and proud of it.",
  "Kwame here - just hit the big 5-0 this year."
)

response <- chat$process(
  prompts,
  type = type_object(
    name = type_string(),
    age = type_number()
  )
)

response$texts()
#>     name age
#> 1   Alex  42
#> 2  Jamal  27
#> 3 Li Wei  19
#> 4 Fatima  35
#> 5 Robert  51
#> 6  Kwame  50
```

### Checkpoint and Resume

Progress is tracked in `response$progress()` and checkpoints are saved to an `.rds` file on the disk, which allows you to easily resume interrupted operations:

``` r
response <- chat$process(prompts, file = "chat.rds")
```

If `file` is not defined, a temporary file will be created by default.

### Sound Notifications

Toggle sound notifications on completion, interruption, and error:

``` r
response <- chat$process(prompts, beep = TRUE)
```

### Verbosity Options

By default, the chat `echo` is set to `FALSE` to show a progress bar. However, you can still configure `echo` by first setting `progress` to `FALSE`:

``` r
prompts <- c(
  "What is R?",
  "Explain base R versus tidyverse"
)

response <- chat$process(
  prompts,
  progress = FALSE,
  echo = TRUE
)
#> R is a programming language and software environment used for 
#> statistical computing and graphics.
#> 
#> Base R consists of the core functionalities built into R, 
#> while tidyverse is a collection of packages that offer a more
#> consistent, readable, and streamlined approach to data manipulation, 
#> visualization, and analysis.
```

### Methods

-   `progress()`: Returns processing status
-   `texts()`: Returns response texts in the same format as the input prompts (i.e., a list if prompts were provided as a list, or a character vector if prompts were provided as a vector). When a `type` is provided, a list with one element for each prompt. When `type` is an consistent object, returns a data frame with one row for each prompt, and one column for each property.
-   `chats()`: Returns a list of chat objects

## Further Reading

-   [Batch and Compare the Similarity of LLM Responses in R](https://dylanpieper.github.io/blog/posts/batch-and-compare-LLM-responses.html) (Blog Post)
