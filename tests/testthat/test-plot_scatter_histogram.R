set.seed(42)
n <- 200
grade_levels <- c("Kindergarten", "1", "2", "3", "4", "5", "6",
                  "7", "8", "9", "10", "11", "12")

test_df <- data.frame(
  user_grade_at_run = factor(sample(grade_levels, n, replace = TRUE)),
  prop_correct = runif(n, 0.3, 1.0),
  median_rt = abs(rnorm(n, 2000, 500))
)

test_that("plot_scatter_histogram returns a marginal histogram plot", {
  p <- suppressMessages(plot_scatter_histogram(test_df))
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_scatter_histogram accepts custom column names", {
  renamed <- test_df
  names(renamed) <- c("grade", "pc", "rt")
  p <- suppressMessages(plot_scatter_histogram(renamed, grade = "grade",
                                               prop_correct = "pc",
                                               median_rt = "rt"))
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_scatter_histogram drops rows with NA grade and reports it", {
  na_df <- test_df
  na_df$user_grade_at_run[1:5] <- NA
  expect_message(plot_scatter_histogram(na_df), "5 removed due to NA in grade")
})

test_that("plot_scatter_histogram validates inputs", {
  expect_error(plot_scatter_histogram("not a df"), "must be a dataframe")
  expect_error(plot_scatter_histogram(test_df[0, ]), "empty")
  expect_error(plot_scatter_histogram(test_df[, c("prop_correct", "median_rt")]),
               "not present.*user_grade_at_run")
  all_na <- test_df
  all_na$median_rt <- NA
  expect_error(plot_scatter_histogram(all_na), "All values in 'median_rt' are NA")
})

test_that("plot_scatter_histogram warns about out-of-range values", {
  bad <- test_df
  bad$prop_correct[1] <- 1.5
  bad$median_rt[2] <- -10
  expect_warning(
    expect_warning(suppressMessages(plot_scatter_histogram(bad)),
                   "outside the range"),
    "negative")
  expect_no_warning(suppressMessages(plot_scatter_histogram(bad, verbose = FALSE)))
})

test_that("plot_scatter_histogram warns about unexpected grade values", {
  odd <- test_df
  odd$user_grade_at_run <- as.character(odd$user_grade_at_run)
  odd$user_grade_at_run[1] <- "Pre-K"
  expect_warning(suppressMessages(plot_scatter_histogram(odd)),
                 "Unexpected grade values found: Pre-K")
})
