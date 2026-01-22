#' Estimate grade using age in months
#'
#' @description
#' If grade is missing or needs to be corrected, use this function to estimate grade based
#' on age in months at the time of the run.
#'
#' @param age_months_at_run The variable that is associated with age in months
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @returns A dataframe with estimated and corrected grades
#' @export
#'
#' @importFrom purrr map_chr
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' test_df <- data.frame(
#'   age_months = c(75, 83, 99, 200),
#'   user_grade = c(NA, "2", "2", "10"),
#'   time_started = c("2025-03-12", "2024-02-17", "2026-01-09", "2025-04-19"))
#' clean_df <- test_df %>% dplyr::mutate(user_grade = dplyr::case_when(
#'   time_started < as.Date("2024-07-31") ~ purrr::map_chr(age_months, estimate_grade),
#'   TRUE ~ user_grade))
#' clean_df <- clean_df %>% dplyr::mutate(user_grade = case_when(
#'   is.na(user_grade) ~ purrr::map_chr(age_months, estimate_grade),
#'   TRUE ~ user_grade))
#' }
estimate_grade <- function(age_months_at_run, verbose = TRUE) {

  # Input validation
  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE/FALSE)")
  }

  # Check if input is NA
  if (is.na(age_months_at_run)) {
    if (verbose) {
      warning("Input age_months_at_run is NA. Returning NA.")
    }
    return(NA_character_)
  }

  # Convert to numeric if character
  if (is.character(age_months_at_run)) {
    age_numeric <- suppressWarnings(as.numeric(age_months_at_run))

    # Check if conversion failed
    if (is.na(age_numeric)) {
      if (verbose) {
        warning("Unable to convert age_months_at_run ('", age_months_at_run,
                "') to numeric. Returning NA.")
      }
      return(NA_character_)
    }

    age_months_at_run <- age_numeric
  }

  # Check if input is numeric (or was successfully converted)
  if (!is.numeric(age_months_at_run)) {
    stop("Input 'age_months_at_run' must be numeric or convertible to numeric")
  }

  # Check for negative values
  if (age_months_at_run < 0) {
    if (verbose) {
      warning("Negative age in months (", age_months_at_run, ") detected. This is likely a data error. Returning NA.")
    }
    return(NA_character_)
  }

  # Check for unrealistic values (e.g., over 120 years old = 1440 months)
  if (age_months_at_run > 1440) {
    if (verbose) {
      warning("Age in months (", age_months_at_run, ") exceeds 120 years. This is likely a data error. Returning NA.")
    }
    return(NA_character_)
  }

  # Warn about very young ages (less than 3 years)
  if (age_months_at_run < 36 && verbose) {
    message("Note: Age is less than 3 years old (", age_months_at_run, " months). Assigning 'Pre-K'.")
  }

  # Warn about college-age or older
  if (age_months_at_run >= 216 && age_months_at_run <= 1440 && verbose) {
    message("Note: Age is ", floor(age_months_at_run / 12), " years (", age_months_at_run,
            " months), which is beyond typical K-12 age range. Assigning grade '12'.")
  }

  # Determine the grade based on age in months
  if (age_months_at_run < 60) {  # Less than 5 years old
    return("Pre-K")
  } else if (age_months_at_run >= 60 && age_months_at_run < 72) {  # 5 years old to (but not including) 6 years old
    return("Kindergarten")
  } else if (age_months_at_run >= 72 && age_months_at_run < 216) {  # From 6 years old to (but not including) 18 years old
    grade <- as.character(floor(age_months_at_run / 12) - 5)  # Grade 1 through 12
    return(grade)
  } else {
    return("12")  # 18 years and older
  }
}
