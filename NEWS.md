# chatalot 0.2.0

## New Features
* Added support for prompts with mixed content (text, images, files) using ellmer content functions

## Internal Improvements
* `$texts()` returns a data frame for structured data when possible
* Added single retry for failed structured data extractions
* Replaced progress bar percentage indicator (`cli::pb_percent`) to estimated time until completion (`cli::pb_eta`)
* For `future_chat()`, updated the default `chunk_size` to a fixed value of 10 prompts
* For `future_chat()`, detect ellmer > 0.2.0 and abort due to API key redaction (temporary until callback is added)

## Lifecycle changes
* Renamed package from hellmer to chatalot
* `lot` is renamed to `process` for consistent verb use
* `batch` is renamed to `lot` to match the new package name
* `state_path` is renamed to `file` for consistency/simplicity
* `type_spec` is renamed to `type` following the latest update to ellmer (0.1.1)
* `seq_chat()` is renamed to `seq_chat()` for brevity
* `future_chat()` is renamed to `future_chat()` for consistency with naming conventions
* Removed evaluation functionality because of poor performance
* Removed retry functionality in anticipation of robust changes to ellmer (development)

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
