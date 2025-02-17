library(hellmer)
library(tidyllm)
library(purrr)
library(tictoc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

run_hellmer_batch <- function() {
  chat <- chat_batch(chat_claude, system_prompt = "Reply concisely")
  result <- chat$batch(list(
    "What is 2+2?",
    "Name one planet.",
    "Is water wet?",
    "What color is the sky?"
  ))
  result$texts()
}

run_hellmer_parallel <- function() {
  chat <- chat_parallel(chat_claude, system_prompt = "Reply concisely")
  result <- chat$batch(list(
    "What is 2+2?",
    "Name one planet.",
    "Is water wet?",
    "What color is the sky?"
  ))
  result$texts()
}

run_tidyllm_batch <- function() {
  conversations <- c(
    "What is 2+2?",
    "Name one planet.",
    "Is water wet?",
    "What color is the sky?"
  ) |>
    purrr::map(~{
      llm_message(.x) |>
        chat(claude())
    })
  conversations
}

n_iterations <- 10
results <- data.frame(
  iteration = numeric(),
  method = character(),
  time_seconds = numeric()
)

for (i in 1:n_iterations) {
  tic()
  run_hellmer_batch()
  hellmer_time <- toc(quiet = TRUE)
  
  tic()
  run_hellmer_parallel()
  hellmer_parallel_time <- toc(quiet = TRUE)
  
  tic()
  run_tidyllm_batch()
  tidyllm_time <- toc(quiet = TRUE)
  
  results <- results |> 
    bind_rows(data.frame(
      iteration = i,
      method = c("hellmer-sequential", "hellmer-parallel", "tidyllm-sequential"),
      time_seconds = c(
        hellmer_time$toc - hellmer_time$tic,
        hellmer_parallel_time$toc - hellmer_parallel_time$tic,
        tidyllm_time$toc - tidyllm_time$tic
      )
    ))
}

summary_stats <- results |>
  group_by(method) |>
  summarise(
    mean_time = mean(time_seconds),
    sd_time = sd(time_seconds),
    median_time = median(time_seconds),
    min_time = min(time_seconds),
    max_time = max(time_seconds)
  )

cat("\nBenchmark Results (seconds):\n")
print(summary_stats)

ggplot(results, aes(x = method, y = time_seconds, fill = method, color = method)) +
  geom_violin(
    width = 0.6,
    alpha = 0.4,
    linewidth = 1,
    bw = .2
  ) +
  geom_boxplot(
    width = 0.2,
    alpha = 0.7,
    size = 1,
    outlier.shape = NA,
    fill = "white"
  ) +
  geom_point(
    position = position_jitter(width = 0.05, seed = 1),
    size = 3,
    alpha = 0.4
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Performance Comparison of Processing Methods",
    x = "Method (Claude)",
    y = "Time (Seconds)"
  )
