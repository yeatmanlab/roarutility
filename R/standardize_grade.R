#' Standardize grade to uniform values
#'
#' @description
#' Create uniform values for grade (e.g., "2", "two", and "2nd" all
#' become "2").
#'
#' @param df The dataframe that has inconsistent grade values
#' @param grade_col The grade variable that needs to be standardized
#' (e.g., "user_grade_at_run" (default))
#'
#' @returns A dataframe with uniform grade values
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rlang :=
standardize_grade <- function(df, grade_col="user_grade_at_run") {
  data_cleaned <- df %>% dplyr::mutate(!!grade_col := case_when(
    !!rlang::sym(grade_col) %in% c("Kindergarten", "K", "Kindergarden", "k") ~ "Kindergarten",
    !!rlang::sym(grade_col) %in% c("1", "1st", "01") ~ "1",
    !!rlang::sym(grade_col) %in% c("2", "2nd", "02") ~ "2",
    !!rlang::sym(grade_col) %in% c("3", "3rd", "03") ~ "3",
    !!rlang::sym(grade_col) %in% c("4", "4th", "04") ~ "4",
    !!rlang::sym(grade_col) %in% c("5", "5th", "05") ~ "5",
    !!rlang::sym(grade_col) %in% c("6", "6th", "06") ~ "6",
    !!rlang::sym(grade_col) %in% c("7", "7th", "07") ~ "7",
    !!rlang::sym(grade_col) %in% c("8", "8th", "08") ~ "8",
    !!rlang::sym(grade_col) %in% c("9", "9th", "09") ~ "9",
    !!rlang::sym(grade_col) %in% c("10", "10th") ~ "10",
    !!rlang::sym(grade_col) %in% c("11", "11th") ~ "11",
    !!rlang::sym(grade_col) %in% c("12", "12th") ~ "12",
    !!rlang::sym(grade_col) %in% c("Invalid", "Other") ~ NA,
    !!rlang::sym(grade_col) %in% c("PK", "Pre-k", "Pre-K", "pre-k", "pre-kindergarten") ~ "Pre-K",
    TRUE ~ !!sym(grade_col)))
  return(data_cleaned)
}
