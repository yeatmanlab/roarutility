test_that("correct_historical_grade aligns earlier grades to the reference year", {
  runs <- data.frame(
    roar_uid = c("A", "A",   # same grade in 23-24 and 24-25 -> updated
                 "B",        # one year only -> unchanged
                 "C", "C",   # already progresses -> unchanged
                 "D", "D",   # reference in 25-26 -> 2 years back
                 "E", "E",   # K in both years -> Pre-K
                 "F", "F"),  # summer (July 2024) run counts as 23-24
    user_grade_at_run = c("5", "5", "3", "Kindergarten", "1", "7", "7",
                          "Kindergarten", "Kindergarten", "2", "3"),
    time_started = c("2023-10-01 10:00:00 UTC", "2024-10-01 10:00:00 UTC",
                     "2023-10-01 10:00:00 UTC",
                     "2023-10-01 10:00:00 UTC", "2024-10-01 10:00:00 UTC",
                     "2024-02-01 10:00:00 UTC", "2025-09-15 10:00:00 UTC",
                     "2024-05-01 10:00:00 UTC", "2024-09-01 10:00:00 UTC",
                     "2024-07-15 10:00:00 UTC", "2024-08-20 10:00:00 UTC"),
    stringsAsFactors = FALSE
  )
  out <- correct_historical_grade(runs, verbose = FALSE)
  expect_equal(out$user_grade_at_run,
               c("4", "5", "3", "Kindergarten", "1", "5", "7",
                 "Pre-K", "Kindergarten", "2", "3"))
  expect_equal(out$grade_corrected,
               c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                 TRUE, FALSE, FALSE, FALSE))
  expect_equal(nrow(out), nrow(runs))
})

test_that("only_if_same_grade = FALSE updates every earlier run", {
  runs <- data.frame(
    roar_uid = c("A", "A"),
    user_grade_at_run = c("2", "5"),   # grades differ, so default leaves it
    time_started = c("2023-10-01", "2024-10-01")
  )
  expect_equal(correct_historical_grade(runs, verbose = FALSE)$user_grade_at_run,
               c("2", "5"))
  expect_equal(correct_historical_grade(runs, only_if_same_grade = FALSE,
                                        verbose = FALSE)$user_grade_at_run,
               c("4", "5"))
})

test_that("non-standard grades and missing anchors are left alone", {
  runs <- data.frame(
    roar_uid = c("A", "A", "B", "B"),
    user_grade_at_run = c("Invalid", "5", "4", NA),
    time_started = c("2023-10-01", "2024-10-01", "2023-10-01", "2024-10-01")
  )
  out <- correct_historical_grade(runs, verbose = FALSE)
  expect_equal(out$user_grade_at_run, c("Invalid", "5", "4", NA))
  expect_false(any(out$grade_corrected))
})

test_that("diagnostics columns are kept on request", {
  runs <- data.frame(roar_uid = "A", user_grade_at_run = "3",
                     time_started = "2024-10-01")
  out <- correct_historical_grade(runs, keep_diagnostics = TRUE, verbose = FALSE)
  expect_true(all(c("school_year", "grade_original", "anchor_year",
                    "anchor_grade", "grade_implied") %in% names(out)))
})
