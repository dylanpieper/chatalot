# hellmer 0.1.2

## Lifecycle changes
* Removed the timeout feature as it's better handled by `option(ellmer_timeout_s = 120)` in ellmer 0.1.1

* Moved parameters from `chat_sequential` and `chat_future` to `$batch()` except for `chat_model` and `...`

* `$batch()` gains `progress` in addition to  `echo` and `...` which are passed to the chat call

# hellmer 0.1.1

## New features
* Removed `structured_data()` method as `texts()` now handles structured data responses

* Updated package documentation for better organization and clarity

## Experimental features
* Structured data extractions support LLM-as-a-judge to refine extracted data via the `judgements` parameter

# hellmer 0.1.0

## New features
* Initial CRAN submission
