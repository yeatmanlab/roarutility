#' Read ROAR data and remove opt-outs
#'
#' @description
#' Read ROAR data using read.csv() and remove opt-outs from the df using the
#' provided Google Drive link. Filters the opt-outs in "PID", "pid",
#' "assessment_pid", "user.assessmentPid" columns.
#'
#'
#' @param data_name Name of the data file (string)
#' @param data_path Path to the directory where data file exists (string)
#' @param opt_out_link Google Drive link to most up-to-date opt-out file (string)
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @return A dataframe with opt-outs removed
#' @export
#'
#' @importFrom googledrive drive_auth drive_download as_id
#' @importFrom utils read.csv
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' new_data <- roar.read.csv("all_runs.csv", "~/Documents",
#' "google.drive.link")
#'
#'
#' # Silent mode
#' new_data <- roar.read.csv("all_runs.csv", "~/Documents",
#' "google.drive.link", verbose = FALSE)
#' }
roar.read.csv <- function(data_name, data_path, opt_out_link, verbose = TRUE){

  # Declare variables to avoid notes on check()
  assessment_pid <- pid <- PID <- user.assessmentPid <- NULL

  # Input validation
  if (missing(data_name) || is.null(data_name) || !is.character(data_name)) {
    stop("'data_name' must be a non-null character string")
  }

  if (missing(data_path) || is.null(data_path) || !is.character(data_path)) {
    stop("'data_path' must be a non-null character string")
  }

  if (missing(opt_out_link) || is.null(opt_out_link) || !is.character(opt_out_link)) {
    stop("'opt_out_link' must be a non-null character string")
  }

  if (!is.logical(verbose)) {
    stop("'verbose' must be TRUE or FALSE")
  }

  # Check if data_path exists
  if (!dir.exists(data_path)) {
    stop(sprintf("Directory does not exist: '%s'", data_path))
  }

  # Check if data file exists
  data_file <- file.path(data_path, data_name)
  if (!file.exists(data_file)) {
    stop(sprintf("Data file does not exist: '%s'", data_file))
  }

  # Read local data file
  tryCatch({
    data <- utils::read.csv(data_file)
  }, error = function(e) {
    stop(sprintf("Failed to read data file '%s': %s", data_file, e$message))
  })

  # Check if data is empty
  if (nrow(data) == 0) {
    if (verbose) warning("The input data file is empty")
    return(data)
  }

  # Authenticate with Google Drive
  tryCatch({
    googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")
  }, error = function(e) {
    stop(sprintf("Google Drive authentication failed: %s", e$message))
  })

  # Download opt-out file from Google Drive to a temporary location
  opt_out_temp <- tempfile(fileext = ".csv")

  tryCatch({
    googledrive::drive_download(
      googledrive::as_id(opt_out_link),
      path = opt_out_temp,
      overwrite = TRUE
    )
  }, error = function(e) {
    stop(sprintf("Failed to download opt-out file from Google Drive: %s\nCheck if the link is valid and accessible.", e$message))
  })

  # Read the opt-out file
  tryCatch({
    opt.out <- readr::read_csv(opt_out_temp, show_col_types = FALSE)
  }, error = function(e) {
    unlink(opt_out_temp)
    stop(sprintf("Failed to read opt-out file: %s", e$message))
  })

  # Delete temporary file
  unlink(opt_out_temp)

  # Check if opt-out file has the required column
  if (!"assessment_pid" %in% colnames(opt.out)) {
    stop("Opt-out file must contain an 'assessment_pid' column")
  }

  # Check if opt-out file is empty
  if (nrow(opt.out) == 0) {
    if (verbose) warning("The opt-out file is empty. No opt-outs will be removed.")
    return(data)
  }

  # Store original row count for reporting
  original_rows <- nrow(data)

  # Check which ID column exists and filter accordingly
  id_column_found <- FALSE

  if("assessment_pid" %in% colnames(data)){
    data <- dplyr::filter(data, !assessment_pid %in% opt.out$assessment_pid)
    id_column_found <- TRUE
    id_column_name <- "assessment_pid"
  } else if("pid" %in% colnames(data)){
    data <- dplyr::filter(data, !pid %in% opt.out$assessment_pid)
    id_column_found <- TRUE
    id_column_name <- "pid"
  } else if("PID" %in% colnames(data)){
    data <- dplyr::filter(data, !PID %in% opt.out$assessment_pid)
    id_column_found <- TRUE
    id_column_name <- "PID"
  } else if("user.assessmentPid" %in% colnames(data)){
    data <- dplyr::filter(data, !user.assessmentPid %in% opt.out$assessment_pid)
    id_column_found <- TRUE
    id_column_name <- "user.assessmentPid"
  }

  # Warn if no recognized ID column was found
  if (!id_column_found) {
    if (verbose) {
      warning("No recognized ID column found ('assessment_pid', 'pid', 'PID', or 'user.assessmentPid'). Returning data without filtering opt-outs.")
    }
    return(data)
  }

  # Calculate and report rows removed
  rows_removed <- original_rows - nrow(data)

  if (verbose) {
    if (rows_removed == 0) {
      message("No opt-outs were found in the data")
    } else {
      message(sprintf("Removed %d opt-out(s) from %d total rows using '%s' column",
                      rows_removed, original_rows, id_column_name))
    }
  }

  # Warn if all data was removed
  if (nrow(data) == 0 && verbose) {
    warning("All rows were removed as opt-outs. The resulting dataframe is empty.")
  }

  # Return the cleaned data without opt-outs
  return(data)
}
