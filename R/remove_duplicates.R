#' Remove duplicate rows
#'
#' @description
#' Removing duplicate rows ensures researchers are accurately counting each unique
#' student, run, or trial.
#'
#' @param df The dataframe that has identical rows
#'
#' @returns A dataframe that contains only unique rows
#' @export
#'
#' @importFrom dplyr distinct
#' @importFrom magrittr %>%
#'
#' @examples
#' test_df <- data.frame(assessment_pid = c("123", "456", NA, "789", "123"),
#'                       roarScore = c(45, 32, 34, 10, 45))
#' clean_df <- remove_duplicates(test_df)
remove_duplicates <- function(df){
  data_cleaned <- df %>% dplyr::distinct()
}
