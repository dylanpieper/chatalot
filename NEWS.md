# hellmer (development)

## Internal Improvements
* Added single retry for failed structured data extractions
* Replaced progress bar percentage indicator (`cli::pb_percent`) to estimated time until completion (`cli::pb_eta`)
* For `chat_future()`, updated the default `chunk_size` to `parallel::detectCores()` for safer processing 

## Lifecycle changes
* `state_path` is renamed to `file` for consistency/simplicity
* `eval_rounds` is renamed to `eval` and now takes a logical value (TRUE/FALSE) instead of a number
* `type_spec` is renamed to `type` following the latest update to ellmer (0.1.1)

# hellmer 0.1.2

## New Features
* `chat_future()` now uses uses CPU cores * 5 as the default chunk size
* `$batch()` gains `progress` in addition to  `echo` and `...` which are passed to the chat call

## Internal Improvements

## Lifecycle changes
* Removed the timeout feature as it's better handled by `option(ellmer_timeout_s = 120)` in ellmer 0.1.1
* Moved parameters from `chat_sequential()` and `chat_future()` to `$batch()` except for `chat_model` and `...`

# hellmer 0.1.1

## New features
* Removed `structured_data()` method as `texts()` now handles structured data responses
* Updated package documentation for better organization and clarity

## Experimental features
* Structured data extractions support judgements to refine extracted data via the `judgements` parameter

# hellmer 0.1.0

## New features
* Initial CRAN submission
