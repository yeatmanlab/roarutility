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
remove_duplicates <- function(df){
  data_cleaned <- df %>% dplyr::distinct()
}
