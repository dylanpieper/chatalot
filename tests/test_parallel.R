chat <- chat_parallel(chat_claude("You reply concisely"), echo = "none") # "text" or "all"

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

batched <- chat$batch(prompts, chunk_size = 2) # resumes if interrupted

batched$texts()

batched$chats()
