#' Align grades across school years for returning students
#'
#' @description
#' For students with runs in more than one school year, makes the grade on
#' earlier runs consistent with the grade recorded in a reference school year
#' (2024-25 by default). This is recommended when working with runs from
#' before the 2024-25 school year.
#'
#' How it works:
#' 1. Works out the school year of every run from `time_started`.
#' 2. For each student, uses the grade on their earliest run in the reference
#'    year or later as the reference grade.
#' 3. Sets the grade on earlier runs to
#'    `reference grade - (reference year - run year)`.
#'
#' By default (`only_if_same_grade = TRUE`), an earlier run is only updated
#' when its grade is the same as the reference grade. Runs whose grade already
#' fits the student's progression, or differs in some other way, are left
#' unchanged and can be reviewed with `keep_diagnostics = TRUE`. Students with
#' no runs in or after the reference year are left unchanged.
#'
#' Run [standardize_grade()] first so grades are one of
#' "Pre-K", "Kindergarten", "1"-"12". Other values are left untouched.
#'
#' @param df A data frame of runs.
#' @param id_col Student identifier column. Default `"roar_uid"`.
#' @param grade_col Grade column to update. Default `"user_grade_at_run"`.
#' @param time_col Run start time (POSIXct, Date, or character such as
#'   `"2024-03-12 15:14:11.432 UTC"`). Default `"time_started"`.
#' @param reference_year Spring year of the school year used as the
#'   reference. Default `2025` (the 2024-25 school year).
#' @param school_year_start_month Month (1-12) a new school year begins.
#'   Default `8`, so July runs count toward the prior school year.
#' @param only_if_same_grade If `TRUE` (default), only update earlier runs
#'   whose grade is the same as the reference grade. If `FALSE`, update every
#'   earlier run for students who have a reference grade.
#' @param keep_diagnostics If `TRUE`, keep helper columns (`school_year`,
#'   `grade_original`, `anchor_year`, `anchor_grade`, `grade_implied`).
#' @param verbose Print a summary of changes. Default `TRUE`.
#'
#' @returns `df` with `grade_col` updated and a logical `grade_corrected`
#'   column marking the rows that changed.
#' @export
#'
#' @importFrom dplyr mutate group_by arrange filter summarise left_join
#' @importFrom dplyr n_distinct if_else row_number across all_of
#' @importFrom rlang .data :=
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' runs <- data.frame(
#'   roar_uid = c("A", "A", "B", "C", "C"),
#'   user_grade_at_run = c("5", "5", "3", "Kindergarten", "1"),
#'   time_started = c("2023-10-01 10:00:00 UTC", "2024-10-01 10:00:00 UTC",
#'                    "2023-10-01 10:00:00 UTC", "2023-10-01 10:00:00 UTC",
#'                    "2024-10-01 10:00:00 UTC"))
#' correct_historical_grade(runs)
#' # A's 2023-24 run becomes "4"; B (one year only) and C are unchanged
#' }
correct_historical_grade <- function(df,
                                     id_col = "roar_uid",
                                     grade_col = "user_grade_at_run",
                                     time_col = "time_started",
                                     reference_year = 2025,
                                     school_year_start_month = 8,
                                     only_if_same_grade = TRUE,
                                     keep_diagnostics = FALSE,
                                     verbose = TRUE) {

  # ---- input validation ----
  if (!is.data.frame(df)) stop("`df` must be a data frame")
  missing_cols <- setdiff(c(id_col, grade_col, time_col), names(df))
  if (length(missing_cols) > 0) {
    stop("Missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  for (arg in c("only_if_same_grade", "keep_diagnostics", "verbose")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1 || is.na(val)) {
      stop("Argument '", arg, "' must be TRUE or FALSE")
    }
  }
  if (!school_year_start_month %in% 1:12) {
    stop("`school_year_start_month` must be an integer from 1 to 12")
  }

  grade_levels <- c("Pre-K", "Kindergarten", as.character(1:12))

  # ---- parse time -> school year (labelled by spring year: 23-24 = 2024) ----
  t <- df[[time_col]]
  if (is.character(t) || is.factor(t)) {
    t <- as.POSIXct(sub(" UTC$", "", as.character(t)), tz = "UTC",
                    tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                                   "%Y-%m-%d"),
                    optional = TRUE)
  }
  if (!inherits(t, c("POSIXt", "Date"))) {
    stop("Could not interpret `", time_col, "` as a date-time")
  }
  yr <- as.integer(format(t, "%Y"))
  mo <- as.integer(format(t, "%m"))
  school_year <- yr + as.integer(mo >= school_year_start_month)

  if (verbose && any(is.na(school_year))) {
    message(sum(is.na(school_year)), " run(s) have an unparseable `",
            time_col, "` and will not be corrected.")
  }

  out <- dplyr::ungroup(df) %>%
    mutate(
      .row_id         = row_number(),
      .t              = t,
      school_year     = school_year,
      grade_original  = .data[[grade_col]],
      .g_num          = match(.data[[grade_col]], grade_levels) - 2L  # Pre-K = -1, K = 0
    )

  # ---- anchor: earliest graded run in the reference year or later ----
  anchors <- out %>%
    filter(.data$school_year >= reference_year, !is.na(.data$.g_num)) %>%
    arrange(.data[[id_col]], .data$.t) %>%
    group_by(dplyr::across(dplyr::all_of(id_col))) %>%
    summarise(anchor_year  = .data$school_year[1],
              anchor_grade = .data$.g_num[1],
              .groups = "drop")

  out <- dplyr::left_join(out, anchors, by = id_col)

  # ---- align earlier runs to the reference grade ----
  out <- out %>%
    mutate(
      .historical   = !is.na(.data$school_year) &
                      .data$school_year < reference_year &
                      !is.na(.data$anchor_year) & !is.na(.data$.g_num),
      grade_implied = .data$anchor_grade - (.data$anchor_year - .data$school_year),
      .fix          = .data$.historical &
                      (!only_if_same_grade | .data$.g_num == .data$anchor_grade),
      # implied grade below Pre-K can't be represented -> leave as-is
      .fix          = .data$.fix & .data$grade_implied >= -1L,
      grade_corrected = .data$.fix & .data$grade_implied != .data$.g_num,
      .idx          = ifelse(.data$grade_implied >= -1L & .data$grade_implied <= 12L,
                             .data$grade_implied + 2L, NA_integer_),
      !!grade_col   := if_else(.data$grade_corrected,
                               grade_levels[.data$.idx],
                               as.character(.data[[grade_col]]))
    )

  # ---- summary ----
  if (verbose) {
    n_hist     <- sum(out$.historical)
    n_fixed    <- sum(out$grade_corrected)
    n_students <- dplyr::n_distinct(out[[id_col]][out$grade_corrected])
    n_mismatch <- sum(out$.historical & !out$.fix &
                        out$grade_implied != out$.g_num, na.rm = TRUE)
    n_below    <- sum(out$.historical & out$grade_implied < -1L, na.rm = TRUE)
    message(sprintf(
      "Earlier runs with a %d-%02d (or later) reference grade: %d | updated: %d (%d students)",
      reference_year - 1L, reference_year %% 100L,
      n_hist, n_fixed, n_students))
    if (n_mismatch > 0) {
      message(n_mismatch, " earlier run(s) differ from the reference grade in ",
              "other ways and were left unchanged. ",
              "Inspect with keep_diagnostics = TRUE.")
    }
    if (n_below > 0) {
      message(n_below, " run(s) would be aligned below Pre-K; left unchanged.")
    }
  }

  # ---- tidy up ----
  out <- out %>% dplyr::arrange(.data$.row_id)
  drop <- c(".row_id", ".t", ".g_num", ".historical", ".fix", ".idx")
  if (!keep_diagnostics) {
    drop <- c(drop, "school_year", "grade_original", "anchor_year",
              "anchor_grade", "grade_implied")
  }
  out[, setdiff(names(out), drop), drop = FALSE]
}
