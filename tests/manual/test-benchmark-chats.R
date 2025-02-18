library(microbenchmark)

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

benchmark_test <- function() {
  chat1 <- chat_future()
  
  chat2 <- chat_sequential()
  
  results <- microbenchmark(
    parallel = chat1$batch(prompts, chunk_size = 1),
    batch = chat2$batch(prompts),
    times = 5
  )
  
  return(results)
}

results <- benchmark_test()
print(results)

boxplot(time/1e9 ~ expr, data = results,
        main = "Performance Comparison",
        ylab = "Time (seconds)",
        xlab = "Implementation")