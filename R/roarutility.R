#' roarutility: Convenience Functions for ROAR Data Processing
#'
#' @description
#' The roarutility package provides a collection of convenience functions for
#' processing and cleaning ROAR (Rapid Online Assessment of Reading) data.
#' This includes functions for removing test, demo, qa, and pilot accounts,
#' for reading in data while simulatenously removing opt-outs, cleaning strings
#' within the dataframes, cleaning demographics, and more.
#'
#' @details
#' Key functions in this package include:
#' \itemize{
#'   \item \code{\link{roar.read.csv}}: Read ROAR data and remove opt-outs.
#'   \item \code{\link{remove_accounts}}: Remove test, demo, pilot, QA, or NA
#'   accounts by selection.
#'   \item \code{\link{clean_strings}}: Remove extra characters from assigning
#'   organization variables and convert empty strings to NA values.
#'   \item \code{\link{remove_empty_cols}}: Remove columns with all NA values.
#'   \item \code{\link{remove_duplicates}}: Remove duplicate rows.
#'   \item \code{\link{estimate_grade}}: Estimates grade using age in months.
#'   \item \code{\link{standardize_grade}}: Standardizes grade to contain uniform values.
#' }
#'
#' @section Typical ROAR Assessment Data Workflow:
#' 1. Load your data using roar.read.csv()
#' 2. Use \code{clean_strings()} to remove extra characters from assigning.
#' organization variables to prepare for merging with the organization key. Also
#' the function converts all empty strings "" to NA values which allows for
#' filtering ease.
#' 3. Use \code{remove_empty_cols()} to remove columns with all NA values to
#' produce a cleaner dataframe with necessary variables only
#' 4. Use \code{remove_duplicates()} to remove duplicate rows and maintain a
#' dataframe with unique observations for clearer counts.
#' 5. Use \code{remove_accounts()} to remove a selection of test, demo, pilot,
#' and QA accounts. Researchers also have the option of removing NA assessment_pid.
#' 6. Use \code{standardize_grade()} to convert grade values to standard uniform values
#' that can be easily filtered, manipulated, and organized.
#' 7. Use \code{estimate_grade()} to estimate missing grade or incorrectly stored
#' grades using age in months.
#' 8. Use \code{filter_assessments()} to select which criteria should
#' be used to filter the assessments (e.g, filtering best_run, reliable,
#' and/or complete assessments for the assessments where these features are active).
#'
#' @examples
#' # Load package
#' library(roarutility)
#'
#' # Using remove_empty_cols
#' test_df <- data.frame(firstname = c("Jane", "John", NA, "Kelly"),
#'                       lastname = c("Doe", NA, NA, "Smith"),
#'                       middlename = c(NA, NA, NA, NA))
#' clean_df <- remove_empty_cols(test_df)
#'
#' # Using clean_strings
#' test_df <- data.frame(assigning_schools = c("[irNgj3c]", "irNgj3c"),
#'                       age = c("", "6.7"))
#' clean_df <- clean_strings(test_df)
#'
#' # Using remove_duplicates
#' test_df <- data.frame(assessment_pid = c("123", "456", NA, "789", "123"),
#'                       roarScore = c(45, 32, 34, 10, 45))
#' clean_df <- remove_duplicates(test_df)
#'
#' # Using standardize_grade
#' test_df <- data.frame(user.grade = c("2", "1", "01", "2nd", "k",
#'                                      "Kindergarten", "1", "09"))
#' clean_df <- standardize_grade(test_df, "user.grade")
#'
#' # Using filter_assessments
#' test_df <- data.frame(task_id = c("roam-alpaca", "swr", "sre", "letter", "sre-es", "swr-es"),
#'                       completed = c("true", "true", "false", "true", "false", "false"),
#'                       best_run = c(NA, "true", "false", "true", NA, NA),
#'                       reliable = c(NA, "true", "false", "true", NA, NA))
#' clean_df <- filter_assessments(test_df, completed=TRUE, best_run=TRUE, reliable=TRUE)
#'
#' \dontrun{
#' # Using roar.read.csv
#' # Read in ROAR data and simulatenously remove opt-outs
#' new_data <- roar.read.csv("all_runs.csv", "~/Documents",
#' "https://drive.google.com/file/d/11gYLqU5xT-NMDxWXGQj8WfZ8AVA_lFT9/view?usp=drive_link")
#'
#' # Using remove_accounts
#' # Remove test and demo accounts
#' clean_data <- remove_accounts(my_data, test = TRUE, demo = TRUE)
#' # Keep everything except NA values
#' clean_data <- remove_accounts(my_data, test = FALSE, demo = FALSE,
#'                                pilot = FALSE, qa = FALSE, na = TRUE)
#'
#' # Using estimate_grade
#' df <- df %>%
#'       mutate(user_grade_at_run = ifelse(time_started < as.Date("2024-07-31"),
#'              map_chr(age_months_at_run, estimate_grade),user_grade_at_run))
#' # estimate grade for anyone with missing grade
#' df <- df %>%
#'       mutate(user_grade_at_run = ifelse(is.na(user_grade_at_run),
#'              map_chr(age_months_at_run, estimate_grade),user_grade_at_run))
#'
#' # Using estimate_grade
#' test_df <- data.frame(
#'   age_months = c(75, 83, 99, 200),
#'   user_grade = c(NA, "2", "2", "10"),
#'   time_started = c("2025-03-12", "2024-02-17", "2026-01-09", "2025-04-19"))
#' clean_df <- test_df %>% mutate(user_grade = case_when(
#'   time_started < as.Date("2024-07-31") ~ map_chr(age_months, estimate_grade),
#'   TRUE ~ user_grade))
#' clean_df <- clean_df %>% mutate(user_grade = case_when(
#'   is.na(user_grade) ~ map_chr(age_months, estimate_grade),
#'   TRUE ~ user_grade))
#' }
#'
#'
#' @author Kelly Wentzlof
#' @keywords roar utility convenience
"_PACKAGE"
