#' Standardize grade to uniform values
#'
#' @description
#' Create uniform values for grade (e.g., "2", "two", and "2nd" all
#' become "2").
#'
#' @param df The dataframe that has inconsistent grade values
#' @param grade_col The grade variable that needs to be standardized
#' (e.g., "user_grade_at_run" (default)) (string)
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @returns A dataframe with uniform grade values
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rlang :=
#'
#' @examples
#' test_df <- data.frame(user.grade = c("2", "1", "01", "2nd", "k",
#'                                      "Kindergarten", "1", "09"))
#' clean_df <- standardize_grade(test_df, "user.grade")
standardize_grade <- function(df, grade_col="user_grade_at_run", verbose=TRUE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  if (!is.character(grade_col) || length(grade_col) != 1) {
    stop("Argument 'grade_col' must be a single character string")
  }

  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE/FALSE)")
  }

  if (nrow(df) == 0) {
    if (verbose) warning("Input dataframe is empty. Returning empty dataframe.")
    return(df)
  }

  # Check if the specified column exists
  if (!grade_col %in% names(df)) {
    stop("Column '", grade_col, "' not found in dataframe. Available columns: ",
         paste(names(df), collapse = ", "))
  }

  # Store initial state
  initial_rows <- nrow(df)
  initial_values <- df[[grade_col]]
  initial_unique <- unique(initial_values[!is.na(initial_values)])

  # Define standardized mappings for reporting
  standardized_mappings <- list(
    "Pre-K" = c("PK", "Pre-k", "Pre-K", "pre-k", "pre-kindergarten"),
    "Kindergarten" = c("Kindergarten", "K", "Kindergarden", "k"),
    "1" = c("1", "1st", "01"),
    "2" = c("2", "2nd", "02"),
    "3" = c("3", "3rd", "03"),
    "4" = c("4", "4th", "04"),
    "5" = c("5", "5th", "05"),
    "6" = c("6", "6th", "06"),
    "7" = c("7", "7th", "07"),
    "8" = c("8", "8th", "08"),
    "9" = c("9", "9th", "09"),
    "10" = c("10", "10th"),
    "11" = c("11", "11th"),
    "12" = c("12", "12th")
  )

  # Identify which values will be changed
  values_changed <- list()
  for (std_grade in names(standardized_mappings)) {
    variants <- standardized_mappings[[std_grade]]
    # Find variants that exist in data but aren't already the standard form
    existing_variants <- intersect(initial_unique, variants)
    non_standard <- setdiff(existing_variants, std_grade)
    if (length(non_standard) > 0) {
      values_changed[[std_grade]] <- non_standard
    }
  }

  # Identify values that will become NA
  invalid_values <- intersect(initial_unique, c("Invalid", "Other"))

  # Identify unrecognized values (not in any mapping and not already NA)
  all_recognized <- c(unlist(standardized_mappings), "Invalid", "Other")
  unrecognized <- setdiff(initial_unique, all_recognized)

  # Perform the standardization
  data_cleaned <- df %>% dplyr::mutate(!!grade_col := dplyr::case_when(
    !!rlang::sym(grade_col) %in% c("Kindergarten", "K", "Kindergarden", "k", "kindergarten") ~ "Kindergarten",
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
    !!rlang::sym(grade_col) %in% c("Invalid", "Other") ~ NA_character_,
    !!rlang::sym(grade_col) %in% c("PK", "Pre-k", "Pre-K", "pre-k", "pre-kindergarten") ~ "Pre-K",
    TRUE ~ !!sym(grade_col)))

  # Count changes
  final_values <- data_cleaned[[grade_col]]
  values_standardized <- sum(initial_values != final_values, na.rm = TRUE)
  new_nas <- sum(is.na(final_values)) - sum(is.na(initial_values))

  # Verbose output
  if (verbose) {
    if (values_standardized > 0 || new_nas > 0 || length(unrecognized) > 0) {
      message("\n--- Grade Standardization Summary ---")

      # Report standardized values
      if (length(values_changed) > 0) {
        message("Standardized grades:")
        for (std_grade in names(values_changed)) {
          variants <- values_changed[[std_grade]]
          message("  ", paste(variants, collapse = ", "), " -> ", std_grade)
        }
      }

      # Report values converted to NA
      if (length(invalid_values) > 0) {
        message("Converted to NA: ", paste(invalid_values, collapse = ", "))
      }

      # Report total changes
      message("\nTotal rows: ", initial_rows)
      message("Values standardized: ", values_standardized)
      if (new_nas > 0) {
        message("New NA values: ", new_nas)
      }
    } else {
      message("No grade values needed standardization")
    }
  }

  # Warnings for unrecognized values
  if (length(unrecognized) > 0 && verbose) {
    warning("Unrecognized grade value(s) found and left unchanged: ",
            paste(unrecognized, collapse = ", "),
            ". Consider reviewing these values.")
  }

  # Warning if all grades become NA
  if (all(is.na(data_cleaned[[grade_col]])) && !all(is.na(initial_values)) && verbose) {
    warning("All grade values are now NA after standardization")
  }

  return(data_cleaned)
}
