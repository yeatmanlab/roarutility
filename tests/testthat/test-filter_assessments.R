test_df <- data.frame(
  task_id = c("roam-alpaca", "swr", "sre", "letter", "sre-es", "swr-es"),
  completed = c("true", "true", "false", "true", "false", "false"),
  best_run = c(NA, "true", "false", "true", NA, NA),
  reliable = c(NA, "true", "false", "true", NA, NA)
)

test_that("filter_assessments properly filters assessments indicated", {
  expect_equal(filter_assessments(test_df)[,2], c("true", "true", "true"))
  expect_equal(filter_assessments(test_df, completed=TRUE, best_run=TRUE, reliable=TRUE)[,2], c("true", "true"))
})
