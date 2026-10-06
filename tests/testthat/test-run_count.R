runs <- data.frame(
  assessment_pid = c("a", "a", "a", "a", "a", "b", "b"),
  run_id         = c("r3", "r1", "r1", "r2", "r4", "r5", "r6"),
  task_id        = c("swr", "swr", "swr", "swr", "pa", "swr", "swr"),
  assessment_stage = c("test", "practice", "test", "test", "test", "test", "test"),
  time_started   = c("2025-09-15 10:00:00 UTC",   # a swr 3rd
                     "2023-10-01 09:00:00 UTC",   # a swr 1st (practice row)
                     "2023-10-01 09:05:00 UTC",   # a swr 1st (test row, same run)
                     "2024-10-01 10:00:00 UTC",   # a swr 2nd
                     "2024-10-02 10:00:00 UTC",   # a pa 1st
                     "2025-01-15 09:00:00 UTC",   # b swr 2nd
                     "2024-01-15 09:00:00 UTC"),  # b swr 1st
  stringsAsFactors = FALSE
)

test_that("run_count numbers runs chronologically per student and task", {
  out <- run_count(runs, verbose = FALSE)
  expect_equal(out$run_count,  c(3L, 1L, 1L, 2L, 1L, 2L, 1L))
  expect_equal(out$total_runs, c(3L, 3L, 3L, 3L, 1L, 2L, 2L))
})

test_that("rows belonging to the same run share one count", {
  out <- run_count(runs, verbose = FALSE)
  r1 <- out[out$run_id == "r1", ]
  expect_equal(nrow(r1), 2)
  expect_equal(unique(r1$run_count), 1L)
})

test_that("original row order and columns are preserved", {
  out <- run_count(runs, verbose = FALSE)
  expect_equal(nrow(out), nrow(runs))
  expect_equal(out$run_id, runs$run_id)
  expect_equal(names(out), c(names(runs), "run_count", "total_runs"))
})

test_that("task_col = NULL counts across all tasks", {
  out <- run_count(runs, task_col = NULL, verbose = FALSE)
  # student a: r1 (2023-10-01), r2 (2024-10-01), r4 pa (2024-10-02), r3 (2025-09-15)
  expect_equal(out$run_count,  c(4L, 1L, 1L, 2L, 3L, 2L, 1L))
  expect_equal(out$total_runs, c(4L, 4L, 4L, 4L, 4L, 2L, 2L))
})

test_that("without a run_id column every row is its own run", {
  no_run <- runs[runs$run_id != "r1", setdiff(names(runs), "run_id")]
  out <- run_count(no_run, verbose = FALSE)
  expect_equal(out$run_count, c(2L, 1L, 1L, 2L, 1L))
})

test_that("runs with missing times are numbered last", {
  na_runs <- runs
  na_runs$time_started[na_runs$run_id == "r6"] <- NA
  out <- run_count(na_runs, verbose = FALSE)
  expect_equal(out$run_count[out$run_id == "r6"], 2L)
  expect_equal(out$run_count[out$run_id == "r5"], 1L)
})

test_that("run_count validates inputs", {
  expect_error(run_count("not a df"), "must be a dataframe")
  expect_error(run_count(runs, id_col = "roar_uid"), "not present")
  expect_error(run_count(runs, verbose = "yes"), "TRUE or FALSE")
})

test_that("run_count reports a summary when verbose", {
  expect_message(run_count(runs), "Counted 6 run\\(s\\) for 2 student\\(s\\)")
})
