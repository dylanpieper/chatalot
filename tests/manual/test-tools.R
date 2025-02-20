square_number <- function(num) {
  num^2
}

chat <- chat_future(chat_openai)

chat$register_tool(tool(
  square_number,
  "Calculates the square of a given number",
  num = type_integer("The number to square")
))

prompts <- list(
  "What is the square of 3?",
  "Calculate the square of 5.",
  "What is 7 squared?",
  "Square of 10?",
  "Find the square of 12.",
  "What is the square of 8 using my tool?",
  "How much is 11 squared?",
  "Square the number 4 for me.",
  "What's the square of 9?",
  "Can you calculate the square of 15?"
)

result <- chat$batch(prompts) # resumes if interrupted

result$progress()
result$texts()
result$chats()
