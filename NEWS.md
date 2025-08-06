# chatalot (development)

## New Features
* `future_chat()` now uses individual chat-based persistence instead of chunk-based processing to prevent data loss
* Added fine-grained status tracking with `chat_status` field to track individual chat completion states

## Internal Improvements
* Replaced chunk-based parallel processing with individual batch processing that preserves completed work on failures
* Added dual-status system maintaining backward compatibility while enabling individual chat recovery
* Improved error isolation - individual chat failures no longer affect completed chats in the same batch

## Lifecycle changes
* Removed `chunk_size` parameter from `future_chat()` as it now processes individual chats in worker-sized batches
* Removed `max_chunk_tries` parameter as individual chat retry logic is handled by ellmer

# chatalot 0.2.0

## New Features
* Depends on ellmer > 0.3.0, and uses the the new API to specify a particular model (e.g., `chat("anthropic/claude-3-5-sonnet-latest")`)
* Added support for prompts with mixed content (text, images, files) using ellmer content functions

## Internal Improvements
* `$texts()` returns a data frame for structured data when possible
* Replaced progress bar percentage indicator (`cli::pb_percent`) to estimated time until completion (`cli::pb_eta`)

## Lifecycle changes
* Renamed package from hellmer to chatalot
* `lot` is renamed to `process` for consistent verb use
* `batch` is renamed to `lot` to match the new package name
* `state_path` is renamed to `file` for simplicity
* `max_chunk_retries` is renamed to `max_chunk_tries` for simplicity (default: 2)
* `type_spec` is renamed to `type` following the latest update to ellmer (0.1.1)
* `chat_sequential()` is renamed to `seq_chat()`
* `chat_future()` is renamed to `future_chat()`
* Removed evaluation functionality due to poor performance
* Removed retry functionality due to new robust retries in ellmer (0.3.0)

# hellmer 0.1.2

## New Features
* `future_chat()` now uses uses CPU cores * 5 as the default chunk size
* `$batch()` gains `progress` in addition to  `echo` and `...` which are passed to the chat call

## Internal Improvements

## Lifecycle changes
* Removed the timeout feature as it's better handled by `option(ellmer_timeout_s = 120)` in ellmer 0.1.1
* Moved parameters from `seq_chat()` and `future_chat()` to `$batch()` except for `chat_model` and `...`

# hellmer 0.1.1

## New features
* Removed `structured_data()` method as `texts()` now handles structured data responses
* Updated package documentation for better organization and clarity


# hellmer 0.1.0

## New features
* Initial CRAN submission
