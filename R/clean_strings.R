#' Clean string variables in ROAR data
#'
#' @description
#' Remove extra characters from assigning organization variables and convert
#' empty strings to NA values.
#'
#'
#' @param df The dataframe that needs to be cleaned
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
clean_strings <- function(df){

  cols_to_clean <- c("assigning_districts", "assigning_schools",
                     "assigning_classes", "assigning_groups",
                     "assigning_families")

  # Filter to only columns that exist in the dataframe
  existing_cols <- cols_to_clean[cols_to_clean %in% names(df)]

  # Remove extra characters from existing assigning_orgs variables
  if (length(existing_cols) > 0) {
    data_cleaned <- df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(existing_cols),
                                  ~stringr::str_remove_all(., "[\\[\\]\\\"\\\\]")))
  } else {
    data_cleaned <- df
  }

  # Substitute empty string ("") values to NA
  data_cleaned <- data_cleaned %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~dplyr::na_if(., "")))

  return(data_cleaned)
}
