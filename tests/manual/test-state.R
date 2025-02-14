chat <- chat_batch()

prompts <- list(
  "What is 2+2?",
  "Name one planet.",
  "Is water wet?",
  "What color is the sky?",
  "Count to 3.",
  "Say hello.",
  "Name a primary color.",
  "What is 5x5?",
  "True or false: Birds can fly.",
  "What day comes after Monday?"
)

result <- chat$batch(prompts, state_path = "test.rds") # resumes if interrupted

result$texts()

result$chats()
