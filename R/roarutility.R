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
#' }
#'
#' @section Typical Workflow:
#' 1. Load your data
#' 2. Use \code{remove_accounts()} to keep real assessment data and accounts
#' 3. Continue with your analysis
#'
#' @examples
#' \dontrun{
#' # Load package
#' library(roarutility)
#'
#' # Read in ROAR data and simulatenously remove opt-outs
#' new_data <- roar.read.csv("all_runs.csv", "~/Documents",
#' "https://drive.google.com/file/d/11gYLqU5xT-NMDxWXGQj8WfZ8AVA_lFT9/view?usp=drive_link")
#' }
#'
#' @examples
#' \dontrun{
#' # Load package
#' library(roarutility)
#'
#' # Remove test and demo accounts
#' clean_data <- remove_accounts(my_data, test = TRUE, demo = TRUE)
#'
#' # Keep everything except NA values
#' clean_data <- remove_accounts(my_data, test = FALSE, demo = FALSE,
#'                                pilot = FALSE, qa = FALSE, na = TRUE)
#' }
#'
#' @author Kelly Wentzlof
#' @keywords utility
"_PACKAGE"
