test_df <- data.frame(
  task_id = c("roam-alpaca", "swr", "sre", "letter", "sre-es", "swr-es", "pa", "letter"),
  completed = c("true", "true", "false", "true", "false", "false", "True", "False"),
  best_run = c(NA, "true", "false", "true", NA, NA, "True", "False"),
  reliable = c(NA, "true", "false", "true", NA, NA, "False", "False")
)

test_that("filter_assessments properly filters assessments indicated", {
  expect_equal(filter_assessments(test_df)[,2], c("true", "true", "true", "true"))
  expect_equal(filter_assessments(test_df, completed=TRUE, best_run=TRUE, reliable=TRUE)[,2], c("true", "true"))
})
