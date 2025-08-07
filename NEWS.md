# chatalot 0.2.0

## New Features
* Depends on ellmer > 0.3.0, and uses the the new API to specify a particular model (e.g., `chat("anthropic/claude-3-5-sonnet-latest")`)
* Added support for prompts with mixed content (text, images, and PDFs) using ellmer content functions

## Internal Improvements
* `future_chat()` now uses persistent caching of individual chats instead of chunks to prevent data loss
* Added enhanced status tracking with `chat_status` to track individual chat completion states
* `$texts()` returns a data frame for structured data when type properties are consistent
* Replaced progress bar percentage indicator (`cli::pb_percent`) to estimated time until completion (`cli::pb_eta`)

## Lifecycle changes
* Renamed package from hellmer to chatalot
* `batch` is renamed to `process` for consistent verb use
* `state_path` is renamed to `file` for simplicity
* `type_spec` is renamed to `type` following ellmer (0.1.1)
* `chat_sequential()` is renamed to `seq_chat()` to match ellmer naming conventions
* `chat_future()` is renamed to `future_chat()` to match ellmer naming conventions
* `process.sequential_chat()` is renamed to `process.seq_chat()` for consistency
* Removed chunking parameters `chunk_size` and `max_chunk_tries` from `future_chat()`
* Removed retry functionality to use new robust retries in ellmer (0.3.0) using `options(ellmer_max_tries)`
* Removed evaluation functionality due to poor performance

# hellmer 0.1.2

## New Features
* `future_chat()` now uses uses CPU cores * 5 as the default chunk size
* `$batch()` gains `progress` in addition to  `echo` and `...` which are passed to the chat call

## Internal Improvements

## Lifecycle changes
* Removed the timeout feature as it's better handled by `option(ellmer_timeout_s = 120)` following ellmer 0.1.1
* Moved parameters from `chat_sequential()` and `chat_future()` to `$batch()` except for `chat_model` and `...`

# hellmer 0.1.1

## New features
* Removed `structured_data()` method as `texts()` now handles structured data responses
* Updated package documentation for better organization and clarity

# hellmer 0.1.0

## New features
* Initial CRAN submission
