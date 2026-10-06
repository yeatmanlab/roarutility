#' Count each student's runs over time
#'
#' @description
#' Numbers each student's runs in chronological order (`run_count`: 1 = first
#' attempt, 2 = second, ...) and adds the student's total number of runs
#' (`total_runs`). By default counts are kept separate for each task, so a
#' student's 2nd SWR run and 2nd PA run both get `run_count = 2`.
#'
#' A single run often spans several rows (e.g. practice and test stages). Rows
#' that share a `run_col` value are treated as one run and get the same count;
#' a run's time is its earliest `time_col` value.
#'
#' @param df A data frame of runs.
#' @param id_col Student identifier column. Default `"assessment_pid"`.
#' @param time_col Run start time (POSIXct, Date, or character such as
#'   `"2024-03-12 15:14:11.432 UTC"`). Default `"time_started"`.
#' @param task_col Task column used to count runs separately per task.
#'   Default `"task_id"`. Use `NULL` to count all of a student's runs together
#'   across tasks.
#' @param run_col Run identifier column. Default `"run_id"`. Use `NULL` (or
#'   leave it out of `df`) to treat every row as its own run.
#' @param verbose Print a summary. Default `TRUE`.
#'
#' @returns `df` in its original row order with two new columns:
#'   `run_count` (the nth run for that student/task) and `total_runs`.
#'   Runs with a missing time are numbered last.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by summarise arrange ungroup left_join
#' @importFrom dplyr row_number n n_distinct across all_of select
#' @importFrom rlang .data
#'
#' @examples
#' runs <- data.frame(
#'   assessment_pid = c("a", "a", "a", "b"),
#'   run_id = c("r2", "r1", "r1", "r3"),
#'   task_id = "swr",
#'   assessment_stage = c("test", "practice", "test", "test"),
#'   time_started = c("2025-03-01 10:00:00", "2024-10-01 10:00:00",
#'                    "2024-10-01 10:00:00", "2025-01-15 09:00:00"))
#' run_count(runs)
run_count <- function(df,
                      id_col = "assessment_pid",
                      time_col = "time_started",
                      task_col = "task_id",
                      run_col = "run_id",
                      verbose = TRUE) {

  # ---- input validation ----
  if (!is.data.frame(df)) stop("Input 'df' must be a dataframe")
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("Argument 'verbose' must be TRUE or FALSE")
  }
  missing_cols <- setdiff(c(id_col, time_col, task_col), names(df))
  if (length(missing_cols) > 0) {
    stop("The following columns are not present in the dataframe: ",
         paste(missing_cols, collapse = ", "))
  }
  use_run_col <- !is.null(run_col) && run_col %in% names(df)
  if (!is.null(run_col) && !use_run_col && verbose) {
    message("Column '", run_col, "' not found; treating each row as one run.")
  }
  if (nrow(df) == 0) {
    return(dplyr::mutate(df, run_count = integer(0), total_runs = integer(0)))
  }

  # ---- parse time ----
  t <- df[[time_col]]
  if (is.character(t) || is.factor(t)) {
    t <- as.POSIXct(sub(" UTC$", "", as.character(t)), tz = "UTC",
                    tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                                   "%Y-%m-%d"),
                    optional = TRUE)
  }
  if (!inherits(t, c("POSIXt", "Date"))) {
    stop("Could not interpret '", time_col, "' as a date-time")
  }

  group_cols <- c(id_col, task_col)

  out <- dplyr::ungroup(df) %>%
    dplyr::mutate(.row_id = dplyr::row_number(),
                  .t = t,
                  .run = if (use_run_col) .data[[run_col]] else .data$.row_id)

  # ---- one row per run, ordered in time within student (and task) ----
  runs <- out %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, ".run")))) %>%
    dplyr::summarise(.first_t = sort(.data$.t)[1], .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(.data$.first_t, .by_group = TRUE) %>%
    dplyr::mutate(run_count = dplyr::row_number(),
                  total_runs = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-".first_t")

  out <- out %>%
    dplyr::left_join(runs, by = c(group_cols, ".run")) %>%
    dplyr::arrange(.data$.row_id)

  if (verbose) {
    n_missing_t <- sum(is.na(out$.t))
    message("Counted ", nrow(runs), " run(s) for ",
            dplyr::n_distinct(out[[id_col]]), " student(s)",
            if (!is.null(task_col)) paste0(" across ",
              dplyr::n_distinct(out[[task_col]]), " task(s)") else "",
            ". Max runs per student",
            if (!is.null(task_col)) "/task" else "", ": ",
            max(out$total_runs), ".")
    if (n_missing_t > 0) {
      message(n_missing_t, " row(s) have a missing '", time_col,
              "'; those runs are numbered last.")
    }
  }

  out[, setdiff(names(out), c(".row_id", ".t", ".run")), drop = FALSE]
}
