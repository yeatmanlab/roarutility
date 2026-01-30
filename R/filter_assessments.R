#' Filter completed, best, and reliable runs
#'
#' @description
#' Filters for completed, best, and reliable runs based on selection. Completed runs
#' are always set to TRUE (default) because this feature is implemented in most
#' (if not all) ROAR assessments. Best and reliables runs are set to FALSE (default)
#' as these are only implemented in the ROAR core assessments.
#'
#'
#' @param df The dataframe that contains all runs including incomplete, non-best,
#'  and unreliable runs
#' @param completed A Boolean value, TRUE (default) or FALSE, that indicates if
#' the researcher wants to remove incomplete runs
#' @param reliable A Boolean value, TRUE or FALSE (default), that indicates if
#' the researcher wants to remove unreliable runs. Note: this feature is only
#' implemented in the ROAR core assessments
#' @param best_run A Boolean value, TRUE or FALSE (default), that indicates if
#' the researchers wants to keep only the best run within each administration.
#' Note: this feature is only implemented in some of the ROAR assessments
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @returns A dataframe that has properly filtered the indicated runs
#' @export
#'
#' @importFrom dplyr filter
#'
#' @examples
#' test_df <- data.frame(
#'            task_id = c("roam-alpaca", "swr", "sre", "letter", "sre-es", "swr-es"),
#'            completed = c("true", "true", "false", "true", "false", "false"),
#'            best_run = c(NA, "true", "false", "true", NA, NA),
#'            reliable = c(NA, "true", "false", "true", NA, NA))
#' clean_df <- filter_assessments(test_df)
#' clean_df <- filter_assessments(test_df, completed=TRUE, best_run=TRUE, reliable=TRUE)
filter_assessments <- function(df, completed=TRUE, reliable=FALSE, best_run=FALSE, verbose=TRUE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  if (!is.logical(completed) || !is.logical(reliable) ||
      !is.logical(best_run) || !is.logical(verbose)) {
    stop("All filter arguments (completed, reliable, best_run, verbose) must be logical (TRUE/FALSE)")
  }

  if (nrow(df) == 0) {
    if (verbose) warning("Input dataframe is empty. Returning empty dataframe.")
    return(df)
  }

  # Store initial state
  initial_rows <- nrow(df)

  # Check for expected columns
  expected_cols <- c("completed", "reliable", "best_run")
  missing_cols <- setdiff(expected_cols, names(df))

  if (length(missing_cols) > 0 && verbose) {
    message("Note: The following columns are not present in the dataframe: ",
            paste(missing_cols, collapse = ", "))
  }

  # Track filtering operations
  filters_applied <- character()
  rows_before_completed <- nrow(df)
  rows_before_reliable <- nrow(df)
  rows_before_best <- nrow(df)

  # create new data frame
  data_cleaned <- df

  # Filter completed
  if(completed) {
    if("completed" %in% names(data_cleaned)) {
      rows_before_completed <- nrow(data_cleaned)
      # Ensure completed values are lowercase
      data_cleaned <- data_cleaned %>%
        dplyr::mutate(completed = tolower(completed))
      data_cleaned <- data_cleaned %>%
        dplyr::filter(completed=="true")

      completed_removed <- rows_before_completed - nrow(data_cleaned)
      if(verbose && completed_removed > 0) {
        message("Removed ", completed_removed, " incomplete run(s)")
      }
      filters_applied <- c(filters_applied, "completed")
    } else if(verbose) {
      warning("Filter 'completed=TRUE' requested, but 'completed' column not found. Skipping this filter.")
    }
  }

  # Filter reliable
  if(reliable==TRUE) {
    if("reliable" %in% names(data_cleaned)) {
      rows_before_reliable <- nrow(data_cleaned)
      # Ensure reliable values are lowercase
      data_cleaned <- data_cleaned %>%
        dplyr::mutate(reliable = tolower(reliable))
      data_cleaned <- data_cleaned %>%
        dplyr::filter(reliable %in% c("true", NA)) # for older core runs, reliable was set to NA for when it was not an active feature

      reliable_removed <- rows_before_reliable - nrow(data_cleaned)
      if(verbose && reliable_removed > 0) {
        message("Removed ", reliable_removed, " unreliable run(s)")
      }
      if(verbose) {
        message("Note: Keeping runs where reliable='true' or NA (for older assessments)")
      }
      filters_applied <- c(filters_applied, "reliable")
    } else if(verbose) {
      warning("Filter 'reliable=TRUE' requested, but 'reliable' column not found. Skipping this filter.")
    }
  }

  # Filter best_run
  if(best_run==TRUE) {
    if("best_run" %in% names(data_cleaned)) {
      rows_before_best <- nrow(data_cleaned)
      # Ensure best_run values are lowercase
      data_cleaned <- data_cleaned %>%
        dplyr::mutate(best_run = tolower(best_run))
      data_cleaned <- data_cleaned %>%
        dplyr::filter(best_run=="true")

      best_removed <- rows_before_best - nrow(data_cleaned)
      if(verbose && best_removed > 0) {
        message("Removed ", best_removed, " non-best run(s)")
      }
      filters_applied <- c(filters_applied, "best_run")
    } else if(verbose) {
      warning("Filter 'best_run=TRUE' requested, but 'best_run' column not found. Skipping this filter.")
    }
  }

  # Calculate total changes
  final_rows <- nrow(data_cleaned)
  total_removed <- initial_rows - final_rows

  # Final summary
  if(verbose) {
    message("\n--- Assessment Filtering Summary ---")

    if(length(filters_applied) > 0) {
      message("Filters applied: ", paste(filters_applied, collapse = ", "))
    } else {
      message("No filters were applied")
    }

    message("Initial rows: ", initial_rows)
    message("Final rows: ", final_rows)
    message("Total removed: ", total_removed, " (",
            round(100 * total_removed / initial_rows, 2), "%)")

    if(total_removed == 0 && length(filters_applied) > 0) {
      message("No rows were removed (all runs met filter criteria)")
    }
  }

  # Warning if all data was removed
  if(final_rows == 0 && initial_rows > 0 && verbose) {
    warning("All rows have been removed. Consider adjusting filter parameters.")
  }

  # Warning if a high percentage was removed
  if(total_removed > 0 && (total_removed / initial_rows) > 0.5 && verbose) {
    warning("More than 50% of rows (", total_removed, " out of ", initial_rows,
            ") were removed. Please verify this is expected.")
  }

  return(data_cleaned)
}
