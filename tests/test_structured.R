chat <- chat_parallel(chat_claude(), echo = "none")

type_sentiment <- type_object(
  "Extract the sentiment scores of a given text",
  positive_score = type_number("Positive sentiment score, 0.0 to 1.0"),
  negative_score = type_number("Negative sentiment score, 0.0 to 1.0"),
  neutral_score = type_number("Neutral sentiment score, 0.0 to 1.0")
)

prompts <- list(
  "I love this product! It's amazing!",
  "This is the worst service ever.",
  "The weather is okay today.",
  "Just received my order and I'm absolutely thrilled with the quality!",
  "Cannot believe how disappointing this experience has been. Complete waste of money.",
  "Meeting went as expected, nothing special to report.",
  "The new update completely transformed my workflow - incredible improvement!",
  "Been using this for a month and honestly can't complain.",
  "Customer support was unhelpful and rude. Never shopping here again.",
  "The conference was pretty standard, met some interesting people.",
  "Blown away by the attention to detail in this design!",
  "Not sure how to feel about the recent changes.",
  "This has exceeded all my expectations - worth every penny!",
  "Frustrated with the constant technical issues and bugs.",
  "The restaurant was decent, food came on time.",
  "Such a pleasure working with this team - they truly go above and beyond!",
  "This implementation is fundamentally flawed and needs a complete overhaul.",
  "Market performance has remained stable this quarter.",
  "Absolutely in love with the new features - game-changing innovation!",
  "Really disappointed with the lack of communication."
)

result <- chat$batch(prompts, type_spec = type_sentiment)

structured_data <- result$structured_data()
