# hellmer 0.1.2

## New Features

* `chat_future()` now uses uses CPU cores * 5 as the default chunk size

* `$batch()` gains `progress` in addition to  `echo` and `...` which are passed to the chat call

## Internal Improvements

* `is_auth_error()` is replaced by `is_retry_error()` which more accurately uses `httr2` errors classes

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
