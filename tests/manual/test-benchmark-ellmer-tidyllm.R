library(hellmer)
library(tidyllm)
library(ellmer)
library(purrr)
library(tictoc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

run_hellmer_batch <- function() {
  chat <- chat_sequential(chat_openai, system_prompt = "Reply concisely")
  result <- chat$batch(list(
    "What is 2+2?",
    "Name one planet.",
    "Is water wet?",
    "What color is the sky?"
  ))
  result$texts()
}

run_hellmer_parallel <- function() {
  chat <- chat_future(chat_openai, system_prompt = "Reply concisely")
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
    purrr::map(~ {
      llm_message(.x) |>
        chat(openai())
    })
  conversations
}

run_ellmer_batch <- function() {
  cht <- ellmer::chat_openai()
  responses <- cht$chat_parallel(list(
    "What is 2+2?",
    "Name one planet.",
    "Is water wet?",
    "What color is the sky?"
  ))
  responses
}

run_ellmer_sequential <- function() {
  # Create a chat instance with echo="none" to disable streaming
  cht <- ellmer::chat_openai(system_prompt = "Reply concisely", echo = "none")

  # Map through each question sequentially, explicitly setting echo="none"
  responses <- purrr::map(
    list(
      "What is 2+2?",
      "Name one planet.",
      "Is water wet?",
      "What color is the sky?"
    ),
    ~ cht$chat(.x, echo = "none")
  )

  responses
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

  tic()
  run_ellmer_batch()
  ellmer_time <- toc(quiet = TRUE)

  tic()
  run_ellmer_sequential()
  ellmer_sequential_time <- toc(quiet = TRUE)

  results <- results |>
    bind_rows(data.frame(
      iteration = i,
      method = c(
        "hellmer-sequential", "hellmer-parallel", "tidyllm-sequential",
        "ellmer-parallel", "ellmer-sequential"
      ),
      time_seconds = c(
        hellmer_time$toc - hellmer_time$tic,
        hellmer_parallel_time$toc - hellmer_parallel_time$tic,
        tidyllm_time$toc - tidyllm_time$tic,
        ellmer_time$toc - ellmer_time$tic,
        ellmer_sequential_time$toc - ellmer_sequential_time$tic
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

results <- results |>
  mutate(
    library = case_when(
      grepl("hellmer", method) ~ "hellmer",
      grepl("tidyllm", method) ~ "tidyllm",
      grepl("ellmer", method) ~ "ellmer",
      TRUE ~ "other"
    ),
    execution_type = case_when(
      grepl("parallel", method) ~ "parallel",
      grepl("sequential", method) ~ "sequential",
      TRUE ~ "unknown"
    )
  )

ggplot(results, aes(x = library, y = time_seconds, fill = execution_type)) +
  geom_violin(
    width = 0.6,
    alpha = 0.4,
    linewidth = 0.5,
    bw = .2,
    position = position_dodge(width = 0.7)
  ) +
  geom_boxplot(
    width = 0.2,
    alpha = 0.7,
    size = 0.5,
    outlier.shape = NA,
    fill = "white",
    position = position_dodge(width = 0.7)
  ) +
  geom_point(
    aes(color = execution_type),
    position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.7, seed = 1),
    size = 2.5,
    alpha = 0.4
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Real-Time Batch Performance",
    x = "Library",
    y = "Time (Seconds)",
    fill = "Execution Type",
    color = "Execution Type"
  )
