# hellmer 0.1.2

## New Features

* `chat_future()` parallel processing improvements:
  * When prompts <= available cores: Uses one worker per prompt and processes all prompts in a single chunk
  * When prompts > available cores: `workers` equals available cores with logarithmic chunk scaling
  * `chunk_size` calculation: `ceiling(n_prompts / (1 + log10(max(1, n_prompts / 10))))`

* `$batch()` gains `progress` in addition to  `echo` and `...` which are passed to the chat call

## Lifecycle changes
* Removed the timeout feature as it's better handled by `option(ellmer_timeout_s = 120)` in ellmer 0.1.1

* Moved parameters from `chat_sequential()` and `chat_future()` to `$batch()` except for `chat_model` and `...`

# hellmer 0.1.1

## New features
* Removed `structured_data()` method as `texts()` now handles structured data responses

* Updated package documentation for better organization and clarity

## Experimental features
* Structured data extractions support LLM-as-a-judge to refine extracted data via the `judgements` parameter

# hellmer 0.1.0

## New features
* Initial CRAN submission
