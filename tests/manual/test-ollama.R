chat <- chat_parallel(
  chat_ollama(
    model = "deepseek-r1:8b",
    system_prompt = "You reply in one sentence or less",
    echo = "none" # required for ollama to suppress output
  ),
  timeout = 1000 # still won't exceed ellmer's timeout
)

prompts <- c(
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

prompts <- as.list(prompts) # works with list or vector

result <- chat$batch(prompts, chunk_size = 5) # resumes if interrupted

result$progress()

result$texts()

result$chats()
