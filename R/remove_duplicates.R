#' Remove duplicate rows
#'
#' @description
#' Removing duplicate rows ensures researchers are accurately counting each unique
#' student, run, or trial.
#'
#' @param df The dataframe that has identical rows
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
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
remove_duplicates <- function(df, verbose = TRUE){

  # Input validation (errors always show regardless of verbose)
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE/FALSE)")
  }

  if (nrow(df) == 0) {
    if (verbose) message("Input dataframe is empty. No duplicates to remove.")
    return(df)
  }

  if (ncol(df) == 0) {
    if (verbose) warning("Input dataframe has no columns. Returning empty dataframe.")
    return(df)
  }

  # Store initial state for reporting
  initial_rows <- nrow(df)

  # Perform the duplicate removal
  data_cleaned <- df %>% dplyr::distinct()

  # Calculate what was removed
  final_rows <- nrow(data_cleaned)
  duplicates_removed <- initial_rows - final_rows

  # Informative messages
  if (verbose) {
    if (duplicates_removed > 0) {
      message("Removed ", duplicates_removed, " duplicate row(s)")
      message("Duplicate percentage: ", round(100 * duplicates_removed / initial_rows, 2), "%")
    } else {
      message("No duplicate rows found")
    }

    message("\n--- Duplicate Removal Summary ---")
    message("Initial rows: ", initial_rows)
    message("Final rows: ", final_rows)
    message("Rows removed: ", duplicates_removed)
  }

  # Warning if a high percentage of rows were duplicates
  if (duplicates_removed > 0 && (duplicates_removed / initial_rows) > 0.5 && verbose) {
    warning("More than 50% of rows (", duplicates_removed, " out of ", initial_rows,
            ") were duplicates. Please verify this is expected and consider investigating the data source.")
  }

  # Warning if all rows were duplicates (only 1 unique row remains)
  if (final_rows == 1 && initial_rows > 1 && verbose) {
    warning("Only 1 unique row remains after removing duplicates. All ",
            initial_rows - 1, " other rows were identical.")
  }

  return(data_cleaned)
}
