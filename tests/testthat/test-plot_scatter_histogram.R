n <- 500
grade_levels <- c("Kindergarten", "1", "2", "3", "4", "5", "6",
                  "7", "8", "9", "10", "11", "12")

test_df <- data.frame(
  user_grade_at_run = factor(
    sample(grade_levels, n, replace = TRUE)
  ),
  prop_correct = runif(n, 0.3, 1.0),
  median_rt = abs(rnorm(n, 2000, 500))
)
plot_scatter_histogram(test_df)

