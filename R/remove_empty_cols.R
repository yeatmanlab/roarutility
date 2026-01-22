#' Remove columns with all NA values
#'
#' @description
#' There are many columns within the ROAR dataframes that only apply to specific assessments.
#' Removing the columns that only contain NA values, allows researchers to isolate
#' important variables.
#'
#'
#' @param df The dataframe that contains extra columns containing only NA values
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
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
remove_empty_cols <- function(df, verbose = TRUE){

  # Input validation (errors always show regardless of verbose)
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE/FALSE)")
  }

  if (nrow(df) == 0) {
    if (verbose) warning("Input dataframe is empty. Returning empty dataframe.")
    return(df)
  }

  if (ncol(df) == 0) {
    if (verbose) warning("Input dataframe has no columns. Returning empty dataframe.")
    return(df)
  }

  # Store initial state for reporting
  initial_cols <- ncol(df)
  initial_col_names <- names(df)

  # Identify columns that are all NA
  all_na_cols <- names(df)[sapply(df, function(x) all(is.na(x)))]

  # Perform the cleaning
  data_cleaned <- df %>% dplyr::select(tidyselect::where(~ !all(is.na(.))))

  # Calculate what was removed
  final_cols <- ncol(data_cleaned)
  cols_removed <- initial_cols - final_cols

  # Informative messages
  if (verbose) {
    if (cols_removed > 0) {
      message("Removed ", cols_removed, " column(s) with all NA values:")
      message("  ", paste(all_na_cols, collapse = ", "))
    } else {
      message("No columns with all NA values found")
    }

    message("\n--- Column Removal Summary ---")
    message("Initial columns: ", initial_cols)
    message("Final columns: ", final_cols)
    message("Columns removed: ", cols_removed)
  }

  # Warning if all columns were removed
  if (ncol(data_cleaned) == 0 && initial_cols > 0) {
    if (verbose) {
      warning("All columns were removed (all contained only NA values). Returning empty dataframe.")
    }
    return(data_cleaned)
  }

  # Warning if a high percentage of columns were removed
  if (cols_removed > 0 && (cols_removed / initial_cols) > 0.5 && verbose) {
    warning("More than 50% of columns (", cols_removed, " out of ", initial_cols,
            ") were removed. Please verify this is expected.")
  }

  return(data_cleaned)
}
