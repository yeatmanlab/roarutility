#' Remove columns with all NA values
#'
#' @description
#' There are many columns within the ROAR dataframes that only apply to specific assessments.
#' Removing the columns that only contain NA values, allows researchers to isolate
#' important variables.
#'
#'
#' @param df The dataframe that contains extra columns containing only NA values
#'
#' @returns A dataframe with columns that contain some non-NA values
#' @export
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
#' test_df <- data.frame(firstname = c("Jane", "John", NA, "Kelly"),
#'                       lastname = c("Doe", NA, NA, "Smith"),
#'                       middlename = c(NA, NA, NA, NA))
#' clean_df <- remove_empty_cols(test_df)
remove_empty_cols <- function(df){
  data_cleaned <- df %>% dplyr::select(where(~ !all(is.na(.))))
}
