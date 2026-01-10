#' Estimate grade using age in months
#'
#' @description
#' If grade is missing or incorrect, use this function to estimate grade based
#' on age in months at the time of the run.
#'
#' @param age_months_at_run The variable that is associated with age in months
#'
#' @returns A dataframe with estimated and corrected grades
#' @export
#'
#' @importFrom purrr map_chr
#' @importFrom dplyr case_when
#'
#' @examples
#' test_df <- data.frame(
#'   age_months = c(75, 83, 99, 200),
#'   user_grade = c(NA, "2", "2", "10"),
#'   time_started = c("2025-03-12", "2024-02-17", "2026-01-09", "2025-04-19"))
#' clean_df <- test_df %>% mutate(user_grade = case_when(
#'   time_started < as.Date("2024-07-31") ~ map_chr(age_months, estimate_grade),
#'   TRUE ~ user_grade))
#' clean_df <- clean_df %>% mutate(user_grade = case_when(
#'   is.na(user_grade) ~ map_chr(age_months, estimate_grade),
#'   TRUE ~ user_grade))
estimate_grade <- function(age_months_at_run) {
  # Determine the grade based on age in months
  if (age_months_at_run < 60) {  # Less than 5 years old
    return("Pre-K")
  } else if (age_months_at_run >= 60 && age_months_at_run < 72) {  # 5 years old to (but not including) 6 years old
    return("Kindergarten")
  } else if (age_months_at_run >= 72 && age_months_at_run < 216) {  # From 6 years old to (but not including) 18 years old
    return(as.character(floor(age_months_at_run / 12) - 5))  # Grade 1 through 12
  } else {
    return("12")  # 18 years and older
  }
}
