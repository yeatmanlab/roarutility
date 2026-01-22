#' Clean string variables in ROAR data
#'
#' @description
#' Remove extra characters from assigning organization variables and convert
#' empty strings to NA values.
#'
#'
#' @param df The dataframe that needs to be cleaned
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @returns A dataframe with NAs rather than empty strings and no extra characters in assigning organization variables
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom stringr str_remove_all
#' @importFrom dplyr na_if
#' @importFrom magrittr %>%
#' @importFrom tidyselect where
#'
#' @examples
#' test_df <- data.frame(assigning_schools = c("[irNgj3c]", "irNgj3c"),
#'                       age = c("", "6.7"))
#' clean_df <- clean_strings(test_df)
clean_strings <- function(df, verbose = TRUE){

  # Input validation (errors always show regardless of verbose)
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  if (nrow(df) == 0) {
    if (verbose) warning("Input dataframe is empty. Returning empty dataframe.")
    return(df)
  }

  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE/FALSE)")
  }

  # Store initial state for reporting
  initial_rows <- nrow(df)
  initial_cols <- ncol(df)

  cols_to_clean <- c("assigning_districts", "assigning_schools",
                     "assigning_classes", "assigning_groups",
                     "assigning_families")

  # Filter to only columns that exist in the dataframe
  existing_cols <- cols_to_clean[cols_to_clean %in% names(df)]
  missing_cols <- setdiff(cols_to_clean, names(df))

  # Inform user about missing columns
  if (length(missing_cols) > 0 && verbose) {
    message("Note: The following assigning organization columns are not present: ",
            paste(missing_cols, collapse = ", "))
  }

  # Inform user about columns that will be cleaned
  if (length(existing_cols) > 0 && verbose) {
    message("Cleaning special characters from: ", paste(existing_cols, collapse = ", "))
  }

  # Count special characters to be removed
  special_char_count <- 0
  if (length(existing_cols) > 0) {
    for (col in existing_cols) {
      special_char_count <- special_char_count +
        sum(grepl("[\\[\\]\\\"\\\\]", df[[col]], perl = TRUE), na.rm = TRUE)
    }
  }

  # Remove extra characters from existing assigning_orgs variables
  if (length(existing_cols) > 0) {
    data_cleaned <- df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(existing_cols),
                                  ~stringr::str_remove_all(., "[\\[\\]\\\"\\\\]")))

    if (verbose && special_char_count > 0) {
      message("Removed special characters from ", special_char_count, " cell(s)")
    }
  } else {
    data_cleaned <- df
    if (verbose) {
      message("No assigning organization columns found to clean")
    }
  }

  # Count empty strings before conversion
  char_cols <- names(data_cleaned)[sapply(data_cleaned, is.character)]

  if (length(char_cols) == 0) {
    if (verbose) {
      message("No character columns found in dataframe")
    }
    return(data_cleaned)
  }

  empty_string_count <- 0
  for (col in char_cols) {
    empty_string_count <- empty_string_count + sum(data_cleaned[[col]] == "", na.rm = TRUE)
  }

  # Substitute empty string ("") values to NA
  data_cleaned <- data_cleaned %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~dplyr::na_if(., "")))

  if (verbose && empty_string_count > 0) {
    message("Converted ", empty_string_count, " empty string(s) to NA")
  }

  # Final summary
  if (verbose) {
    message("\n--- String Cleaning Summary ---")
    message("Rows processed: ", initial_rows)
    message("Character columns processed: ", length(char_cols))

    if (special_char_count == 0 && empty_string_count == 0) {
      message("No changes were made to the data")
    }
  }

  # Warn if resulting dataframe has all NAs in character columns
  all_na_cols <- char_cols[sapply(data_cleaned[char_cols], function(x) all(is.na(x)))]
  if (length(all_na_cols) > 0 && verbose) {
    warning("The following character column(s) are now entirely NA: ",
            paste(all_na_cols, collapse = ", "))
  }

  return(data_cleaned)
}
