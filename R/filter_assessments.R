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
#'  and unreliable runs.
#' @param completed A string value, TRUE (default) or FALSE, that indicates if
#' the researcher wants to remove incomplete runs.
#' @param reliable A Boolean value, TRUE or FALSE (default), that indicates if
#' the researcher wants to remove unreliable runs. Note: this feature is only
#' implemented in the ROAR core assessments.
#' @param best_run A Boolean value, TRUE or FALSE (default), that indicates if
#' the researchers wants to keep only the best run within each administration.
#' Note: this feature is only implemented in some of the ROAR assessments.
#'
#' @returns A dataframe that has properly filtered the indicated runs.
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
filter_assessments <- function(df, completed=TRUE, reliable=FALSE, best_run=FALSE) {

  # create new data frame
  data_cleaned <- df

  if(completed && "completed" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(completed=="true")
  }

  if(reliable==TRUE && "reliable" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(reliable %in% c("true", NA)) # for older core runs, reliable was set to NA for when it was not an active feature
  }

  if(best_run==TRUE && "best_run" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(best_run=="true")
  }

  return(data_cleaned)
}
