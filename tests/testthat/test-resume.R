create_resume_test_data <- function() {
  data_path <- system.file("data_manifestos.rda", package = "chatalot")
  load(data_path)

  immig_type <- type_object(
    party = type_string("The name of the political party."),
    immig_score = type_integer("An integer position from -1 to 1 on immigration and
                               asylum seekers, where:
                               -2 means strongly against any immigration,
                               -1 means moderately opposed,
                               0 means neutral,
                               1 means moderately in favor of a more permissive stance on immigration,
                               2 means strongly in favour of a permissive stance on immmigration."),
    immig_rationale = type_string("A brief rationale for your answer.")
  )

  list(
    data = data_manifestos,
    type = immig_type
  )
}

create_oversized_data <- function(test_data) {
  test_data$data["Lab_2010"] <- paste(
    rep(test_data$data["Lab_2010"], 4),
    collapse = "\n\n---\n\n"
  )
  test_data
}

test_resume_with_modified_prompt <- function(chat_constructor, processor_args = list()) {
  skip_chat_test()

  test_data <- create_resume_test_data()
  oversized_data <- create_oversized_data(test_data)

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))

  chat <- create_test_chat(chat_constructor,
    model = "openai/gpt-4o",
    system_prompt = "Extract structured data from political texts. You will be scoring the position of each party on immigration and asylum seekers."
  )

  args_error <- c(
    list(oversized_data$data,
      type = oversized_data$type,
      file = temp_file,
      beep = FALSE
    ),
    processor_args
  )
  expect_error(do.call(chat$process, args_error))

  test_data$data["Random_9999"] <- test_data$data["Lab_2010"]

  args_success <- c(
    list(test_data$data,
      type = test_data$type,
      file = temp_file,
      beep = FALSE
    ),
    processor_args
  )
  result <- do.call(chat$process, args_success)

  expect_equal(nrow(result$texts()), 10)
}

test_resume_with_removed_prompt <- function(chat_constructor, processor_args = list()) {
  skip_chat_test()

  test_data <- create_resume_test_data()
  oversized_data <- create_oversized_data(test_data)

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))

  chat <- create_test_chat(chat_constructor,
    model = "openai/gpt-4o",
    system_prompt = "Extract structured data from political texts. You will be scoring the position of each party on immigration and asylum seekers."
  )

  args_error <- c(
    list(oversized_data$data,
      type = oversized_data$type,
      file = temp_file,
      beep = FALSE
    ),
    processor_args
  )
  expect_error(do.call(chat$process, args_error))

  removed_data <- test_data$data[!names(test_data$data) %in% c("SNP_2010")]

  args_success <- c(
    list(removed_data,
      type = test_data$type,
      file = temp_file,
      beep = FALSE
    ),
    processor_args
  )
  result <- do.call(chat$process, args_success)

  expect_equal(nrow(result$texts()), 8)
}

# these tests are too expensive to automate

# test_that("future_chat resumes with modified and added prompts", {
#   test_resume_with_modified_prompt(future_chat, list(workers = 3))
# })
#
# test_that("seq_chat resumes with modified and added prompts", {
#   test_resume_with_modified_prompt(seq_chat)
# })
#
# test_that("future_chat resumes with removed prompts", {
#   test_resume_with_removed_prompt(future_chat, list(workers = 3))
# })
#
# test_that("seq_chat resumes with removed prompts", {
#   test_resume_with_removed_prompt(seq_chat)
# })
