#' Read ROAR data and remove opt-outs
#'
#' @description
#' Read ROAR data using read.csv() and remove opt-outs from the df using the
#' provided Google Drive link. Filters the opt-outs in "PID", "pid",
#' "assessment_pid", "user.assessmentPid" columns.
#'
#'
#' @param data_name Name of the data file
#' @param data_path Path to the data file
#' @param opt_out_link Google Drive link to most up-to-date opt-out file
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
#' "https://drive.google.com/file/d/11gYLqU5xT-NMDxWXGQj8WfZ8AVA_lFT9/view?usp=drive_link")
#' }
roar.read.csv <- function(data_name, data_path, opt_out_link){

  # Declare variables to avoid notes on check()
  assessment_pid <- pid <- PID <- user.assessmentPid <- NULL

  # Read local data file
  data <- utils::read.csv(file.path(data_path, data_name))

  # Authenticate with Google Drive
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")

  # Download opt-out file from Google Drive to a temporary location
  opt_out_temp <- tempfile(fileext = ".csv")
  googledrive::drive_download(
    googledrive::as_id(opt_out_link),
    path = opt_out_temp,
    overwrite = TRUE
  )

  # Read the opt-out file
  opt.out <- readr::read_csv(opt_out_temp, show_col_types = FALSE)

  # Delete temporary file
  unlink(opt_out_temp)

  # Check which ID column exists and filter accordingly
  if("assessment_pid" %in% colnames(data)){
    data <- dplyr::filter(data, !assessment_pid %in% opt.out$assessment_pid)
  } else if("pid" %in% colnames(data)){
    data <- dplyr::filter(data, !pid %in% opt.out$assessment_pid)
  } else if("PID" %in% colnames(data)){
    data <- dplyr::filter(data, !PID %in% opt.out$assessment_pid)
  } else if("user.assessmentPid" %in% colnames(data)){
    data <- dplyr::filter(data, !user.assessmentPid %in% opt.out$assessment_pid)
  }

  # Return the cleaned data without opt-outs
  return(data)
}
