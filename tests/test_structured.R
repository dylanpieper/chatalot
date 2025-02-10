chat <- chat_batch(chat_claude(), echo = "all")

type_sentiment <- type_object(
  "Extract the sentiment scores of a given text",
  positive_score = type_number("Positive sentiment score, 0.0 to 1.0"),
  negative_score = type_number("Negative sentiment score, 0.0 to 1.0"),
  neutral_score = type_number("Neutral sentiment score, 0.0 to 1.0")
)

prompts <- list(
  "I love this product! It's amazing!",
  "This is the worst service ever.",
  "The weather is okay today."
)

result <- chat$batch(prompts, type_spec = type_sentiment)

structured_data <- result$structured_data()
