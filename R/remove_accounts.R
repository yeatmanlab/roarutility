#' Remove test, demo, pilot, QA, or NA accounts
#'
#' @description
#' Removes all indicated accounts from the dataframe. Reseearchers can
#' indicate which or all of the following types of accounts they would
#'  like to remove from the dataframe. The function defaults to removing test,
#'  demo, pilot, and QA accounts and defaults to not removing NA assessment_pid.
#'   The function combs through the organization IDs
#'   (i.e., assigning_districts, etc.). It also uses string detection to
#'   determine if there are any "test", "pilot", "qa", or "demo" strings
#'   within the assessment_pid column. Finally, it runs through to determine
#'   if there are test or demo accounts using the variables is_test_data and is_demo_data.
#'   If selected, the function
#'    will also remove assessment_pid = NA.
#'
#' @param df The dataframe that contains all types of accounts
#' @param test A Boolean value, TRUE (default) or FALSE, that indicates whether
#' or not to remove test accounts/participants
#' @param demo A Boolean value, TRUE (default) or FALSE, that indicates whether
#' or not to remove demo accounts/participants
#' @param pilot A Boolean value, TRUE (default) or FALSE, that indicates whether
#' or not to remove pilot accounts/participants
#' @param qa A Boolean value, TRUE (default) or FALSE, that indicates whether
#' or not to remove QA accounts/participants
#' @param na A Boolean value, TRUE or FALSE (default), that indicates whether
#' or not to remove NA assessment_pid/accounts/participants
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @returns A dataframe with the indicated remaining account types
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
remove_accounts <- function(df, test=TRUE, demo=TRUE, pilot=TRUE, qa=TRUE, na=FALSE, verbose=TRUE){

  # Input validation (errors always show regardless of verbose)
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  if (nrow(df) == 0) {
    if (verbose) warning("Input dataframe is empty. Returning empty dataframe.")
    return(df)
  }

  if (!is.logical(test) || !is.logical(demo) || !is.logical(pilot) ||
      !is.logical(qa) || !is.logical(na) || !is.logical(verbose)) {
    stop("All filter arguments (test, demo, pilot, qa, na, verbose) must be logical (TRUE/FALSE)")
  }

  # Store initial row count
  initial_rows <- nrow(df)

  # set new dataframe
  data_cleaned <- df

  # Track rows before each filter
  rows_before_test <- nrow(data_cleaned)
  rows_before_demo <- nrow(data_cleaned)
  rows_before_pilot <- nrow(data_cleaned)
  rows_before_qa <- nrow(data_cleaned)
  rows_before_na <- nrow(data_cleaned)

  # Declare variables to avoid notes on check()
  is_test_data <- is_demo_data <- assessment_pid <- assigning_districts <-
    assigning_groups <- assigning_schools <- NULL

  # Check for expected columns and warn if missing
  expected_cols <- c("assessment_pid", "assigning_districts", "assigning_schools",
                     "assigning_groups", "is_test_data", "is_demo_data")
  missing_cols <- setdiff(expected_cols, names(df))

  if (length(missing_cols) > 0 && verbose) {
    message("Note: The following columns are not present in the dataframe: ",
            paste(missing_cols, collapse = ", "))
    message("Filtering will be limited to available columns only.")
  }

  # Known test orgs
  # known assigning_districts used for testing
  test_districts = c("kXyCT8BbFFbuXo5u0M84",
                     "5JEii1roHs6wYxBefH7i",
                     "8h2DYHcFeL8b2n63imYj",
                     "cdfdtUE7YlVHNetPjpLH",
                     "EZoxQN58Ef0FplJirb4E",
                     "i6rlAHxz5Ba7ODBrURMJ",
                     "I9ln6aBe1kQna79kKTyN",
                     "igMn4c109PdqRoYOpLv2",
                     "jjNaHJalCbP8Hl3sSVom",
                     "jt3TWcTy5HqVijv9vgpm",
                     "lnb4UCExEygNUx8dQ7Nd",
                     "MtLkvZGc8TDtZKEVDFuw",
                     "NS8XLWGLYsCUKb4cN282",
                     "ouk1yxYifdYXEATb7ay2",
                     "P81PMPf9jiihidpSDH9G",
                     "qlwGGNfaOTuU5bT0IQNI",
                     "Tmhm9rxxMRnzKsfTxmB3",
                     "u6Jm-2FrnDCXI-3D--0",
                     "US4pYG1gdxbztTweBUDF",
                     "vts6CYd4c00fStm3zmpu",
                     "x3OgZQK9NRLa5hkjpaTR",
                     "xD0qgRE8RyWz6Wx5nznw",
                     "yGGlUDTB5xzzCecoYucj",
                     "ZgiBoZW8Zo6w8IQ5kwwV",
                     "zHezbB3WkQ8FtDyUZCrC",
                     "x3Hd2agh5-2Bw-3D--10",
                     "x3aJySbASzK8KIjEZgJt")

  # known assigning_groups used for testing
  test_groups = c("xCfln1aBgGISoxhRhwax", # blank
                  "yettgcgO25iX1uupiEgw", # Test Group
                  "v1B0A4BY7uVQt4YwEMod", # LEVANTE-CORE-TASKS-TEST
                  "S91j0taJd1rXCEXhbSnB", # zzzTG
                  "7dX5tAGAFXkdwQ4idPms", # aerdf Demo
                  "X7GFqFVDyLFDdun4TqP7")

  # knowning assigning_schools used for testing
  test_schools = c("03xEaKoFXSe6QSvNe5Q2",
                   "61e9fd89d016951095f43c65",
                   "61e9fd89d016951095f43c68",
                   "G2mM3eEwDPZU3h2A4ikZ",
                   "RgrPvxn5kkKSal6wdpQy",
                   "RqAKFNEVjBrZXvSE7OsC",
                   "rZRPSq6IfUOkAQM2mecC",
                   "IHPMiPdfB0UL1mD6KIm9",
                   "FYUQ8HxsPqCQItYhOEH8",
                   "fhWRM3h3EJChE2C1DXkq",
                   "zPJwNdftKI7a1M4y0k4q",
                   "HqgNO9DKUKrfWZUQtl2L",
                   "Giy1vfAON9hHHQ0DJMbk",
                   "kXyCT8BbFFbuXo5u0M84",
                   "h7ZM60k23YVDVm0IMIcB",
                   "3Be5rnRws74NngFdemrB",
                   "8C507FJp1BfoWY9FUE6Q",
                   "YwXfxKUHuoWNOADBEfty",
                   "8fmknC5ZQa8duD13deY5",
                   "676n6YI6tjwgawbGSSNo",
                   "nEqr1fgq39ZLzZURtUzw",
                   "IsXWcHy3fBNUSfriE1V9",
                   "1keq1ApOGJg7cPIruVl8",
                   "aZJB05nC2gJI4GJaPabx",
                   "J1kuSTr5UzufcjkJjMHf",
                   "u6Jm-2FrnDCXI-3D--610",
                   "uSpviE1t4lUzH8J7I9aQ",
                   "1TEvPLoNqG2fZGSAQ0nb",
                   "I2eopueMc6ta5EhwVA0y",
                   "GZk1zSsqaCB1dq1mCZPR",
                   "T2OxlYrQWo0AxkEa3PFA",
                   "Vw72kWhrTrFdlhcVojW0",
                   "01ZQM6h2oHRBYWjdorfJ",
                   "x3Hd2agh5-2Bw-3D--17",
                   "x3Hd2agh5-2Bw-3D--3971-2",
                   "x3Hd2agh5-2Bw-3D--QATeam",
                   "x3Hd2agh5-2Bw-3D--3971",
                   "x3Hd2agh5-2Bw-3D--12",
                   "x3Hd2agh5-2Bw-3D--QATeam-2",
                   "x3Hd2agh5-2Bw-3D--13")

  # Known demo orgs
  # known assigning_districts used for demoing
  demo_districts = c("41JfwqHsCUSa0nH6Q4fR",
                     "61e8aee84cf0e71b14295d45",
                     "cdfdtUE7YlVHNetPjpLH",
                     "iCsc6D2XeJvZbEst1UlX",
                     "kXyCT8BbFFbuXo5u0M84",
                     "q32L2H3qMkBzwKoCfHAw",
                     "rVE6PjkuQlSliSEMhCwH",
                     "rZRPSq6IfUOkAQM2mecC",
                     "sknRRR2TOpNsW8bI7qgF",
                     "sprIUWU0Vq3id38cv6Wn",
                     "TAWCgluNkAApj1P9pYXV",
                     "tFmYlaDMNVDE53swY2yU",
                     "uP9yQCfe1hVIhOmulnqq",
                     "vtORpwcrbjWz0vQByZsm",
                     "W2LndgWeucXOjCSqbU7l",
                     "wvacQ7s9pJVAlSprmz6C",
                     "ZnBaGxwpuge6WDdGQcrV",
                     "x3Hd2agh5-2Bw-3D--10",
                     "x3aJySbASzK8KIjEZgJt")

  # knowing assinging_schools used for demoing
  demo_schools = c("jo3irt0W7KiyFGp3OlDw",
                   "61e9fd89d016951095f43c6a",
                   "61e9fd89d016951095f43c63",
                   "61e9fd89d016951095f43c67",
                   "61e9fd89d016951095f43c6b",
                   "61e9fd89d016951095f43c65",
                   "61e9fd89d016951095f43c68",
                   "61e9fd89d016951095f43c60",
                   "61e9fd89d016951095f43c61",
                   "61e9fd89d016951095f43c5f",
                   "61e9fd89d016951095f43c64",
                   "61e9fd89d016951095f43c62",
                   "61e9fd89d016951095f43c71",
                   "61e9fd89d016951095f43c75",
                   "61e9fd89d016951095f43c77",
                   "61e9fd89d016951095f43c5e",
                   "61e9fd89d016951095f43c76",
                   "61e9fd89d016951095f43c69",
                   "61e9fd89d016951095f43c6d",
                   "61e9fd89d016951095f43c72",
                   "61e9fd89d016951095f43c70",
                   "61e9fd89d016951095f43c6f",
                   "61e9fd89d016951095f43c66",
                   "61e9fd89d016951095f43c74",
                   "61e9fd89d016951095f43c73",
                   "61e9fd89d016951095f43c6c",
                   "61e9fd89d016951095f43c6e",
                   "61e9fd89d016951095f43c78",
                   "AqOjQboh14S4RHTu5KEW",
                   "xdObQ92HT8fKdXb4eyTF",
                   "fqdaCZqjGTYqUmlXrbK3",
                   "AQQaIHjbem5uyhXVm09S",
                   "U4tG6bUgm55Gkbd1Bkyv",
                   "faPUhVGdRAYqFf1otpht",
                   "7girI7eL4Zyx7KdmZHc7",
                   "E1xEowNGuFR8DKkEKn9x",
                   "TEXecqKddYumF5i0NVPq",
                   "ywDFkRzoEJoyyd4ZLLoq",
                   "9Z9hlkC4kk7XGmBSi3gi",
                   "0pW7T4KHqbt1tD1vbyKl",
                   "3mpYIykskSz7SwiJoux3",
                   "i6xFYS7l9SqeJmYe2UVq",
                   "F92Jwi408kpV0Wp49g9t",
                   "R6ipyPuvXP5wlyfLdTl6",
                   "84hFVIogAC2fAMl28Rdi",
                   "EFW88Br2XbECRhA7o7IM",
                   "aJc3CtXAuR228KA4B6M9",
                   "U6PsDZwzkOmsM3Oar2HS",
                   "h7ZM60k23YVDVm0IMIcB",
                   "iz8OQin6RPt6rYe5EVgw",
                   "FcWikzx91G0tQj41Lx4M",
                   "28q0lcmPFyvqbS11iyP0",
                   "jeQxRxloRPZcwoAWosGy",
                   "RgrPvxn5kkKSal6wdpQy",
                   "j87f5dDJJ7OMshXPom2W",
                   "v4QcWQAKMu6cVQiTWk17",
                   "hXdEjHpDUQq81Sxvhc9n",
                   "yCMhgJFUXubGBxhj2xEf",
                   "dx9NUn3EFDYzTIkKjdEH",
                   "Xxa558FwFL55NkCeMn9D",
                   "OyPxOyJ5ounw7JR9jyqY",
                   "PqxLmfVlnyDzIhIJG4AK",
                   "RqAKFNEVjBrZXvSE7OsC",
                   "ek3MTy3CJxEhDIFHCZpz",
                   "03xEaKoFXSe6QSvNe5Q2",
                   "LPtt5Uk6z4JmXkwD3ORz",
                   "B8v9soZa25zEMBuJyMqz",
                   "fQVZ7xrHevzFQwP5PpnP",
                   "ch8UaktMxo8wxoPZFw38",
                   "HMPjM68X1giHnXgr2We9",
                   "O3npRB3Y1pJ3ZHL9d8vZ",
                   "rMTIvUGVcmfMeDwqJ1dj",
                   "LFWLjf5rOhhfaR6FN3SZ",
                   "K25kFMQDDBD2qzV7b4VW",
                   "McVoNmHJswnjvXMJMRxj",
                   "exXEwywVvcbKouVT2Fwh",
                   "EbnKGFddlsOX24ECItbE",
                   "NkqcvkjVTifZ8Wj0lLKA",
                   "xWHO69Paw6Y7fhCZ8zNG",
                   "tilIAxmMt3mhzsTBvCWw",
                   "AACy4w1Vx3Owq1tdXsJY",
                   "0jfKFsYQw9jDL2mwRElt",
                   "iSq112EmQl1m9Mpn0Kr7",
                   "X37EujapvM3RTynznIYt",
                   "p6ENDUxAJSlUaSeBvsSO",
                   "Vv4OYigXPe8tOvUu1PGW",
                   "pkcuMirWv2dwfnLxxe94",
                   "4LclFTcrpudam5UhReac",
                   "hpG8Zv83FyFKfpl1Dqdc",
                   "ydvRYzEsAAj4gFmMbHVx",
                   "01ZQM6h2oHRBYWjdorfJ",
                   "x3Hd2agh5-2Bw-3D--17",
                   "x3Hd2agh5-2Bw-3D--3971-2",
                   "x3Hd2agh5-2Bw-3D--QATeam",
                   "x3Hd2agh5-2Bw-3D--3971",
                   "x3Hd2agh5-2Bw-3D--12",
                   "x3Hd2agh5-2Bw-3D--QATeam-2",
                   "x3Hd2agh5-2Bw-3D--13")

  # Known pilot orgs
  # known assigning_districts used for piloting
  pilot_districts = c("bm6N82WFObiurGAKeJUv",
                      "rR8Ezi7hXlh6pay9G77p",
                      "x3Hd2agh5-2Bw-3D--10",
                      "x3aJySbASzK8KIjEZgJt")

  # knowing assinging_schools used for piloting
  pilot_schools = c("R0CNy2vWCLFxju2LKwpK",
                    "YkeT3QDF9uTcqgzCb936",
                    "06fkRGiViGxzREGYAiGr",
                    "01ZQM6h2oHRBYWjdorfJ",
                    "x3Hd2agh5-2Bw-3D--17",
                    "x3Hd2agh5-2Bw-3D--3971-2",
                    "x3Hd2agh5-2Bw-3D--QATeam",
                    "x3Hd2agh5-2Bw-3D--3971",
                    "x3Hd2agh5-2Bw-3D--12",
                    "x3Hd2agh5-2Bw-3D--QATeam-2",
                    "x3Hd2agh5-2Bw-3D--13")

  # Known qa orgs
  # known assigning_districts used for qa testing
  qa_districts = c("x3aJySbASzK8KIjEZgJt",
                   "x3Hd2agh5-2Bw-3D--10",
                   "xD0qgRE8RyWz6Wx5nznw")

  # knowing assinging_schools used for qa testing
  qa_schools = c("01ZQM6h2oHRBYWjdorfJ",
                 "x3Hd2agh5-2Bw-3D--17",
                 "x3Hd2agh5-2Bw-3D--3971-2",
                 "x3Hd2agh5-2Bw-3D--QATeam",
                 "x3Hd2agh5-2Bw-3D--3971",
                 "x3Hd2agh5-2Bw-3D--12",
                 "x3Hd2agh5-2Bw-3D--QATeam-2",
                 "x3Hd2agh5-2Bw-3D--13",
                 "I2eopueMc6ta5EhwVA0y")

  # Apply filtering

  # if test==TRUE, remove test accounts
  if(test) {
    rows_before_test <- nrow(data_cleaned)

    if("is_test_data" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(is_test_data == "false")
    }

    if("assigning_districts" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_districts %in% test_districts))
    }

    if("assigning_schools" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_schools %in% test_schools))
    }

    if("assigning_groups" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_groups %in% test_groups))
    }

    if("assessment_pid" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!str_detect(tolower(assessment_pid), "test|zzz"))
    }

    test_removed <- rows_before_test - nrow(data_cleaned)
    if(verbose && test_removed > 0) {
      message("Removed ", test_removed, " test account(s)")
    }
  }

  # if demo==TRUE, remove demo accounts
  if(demo) {
    rows_before_demo <- nrow(data_cleaned)

    if("is_demo_data" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(is_demo_data == "false")
    }

    if("assigning_districts" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_districts %in% demo_districts))
    }

    if("assigning_schools" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_schools %in% demo_schools))
    }

    if("assessment_pid" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!str_detect(tolower(assessment_pid), "demo"))
    }

    demo_removed <- rows_before_demo - nrow(data_cleaned)
    if(verbose && demo_removed > 0) {
      message("Removed ", demo_removed, " demo account(s)")
    }
  }

  # if pilot==TRUE, remove pilot accounts
  if(pilot) {
    rows_before_pilot <- nrow(data_cleaned)

    if("assigning_districts" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_districts %in% pilot_districts))
    }

    if("assigning_schools" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_schools %in% pilot_schools))
    }

    if("assessment_pid" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!str_detect(tolower(assessment_pid), "pilot"))
    }

    pilot_removed <- rows_before_pilot - nrow(data_cleaned)
    if(verbose && pilot_removed > 0) {
      message("Removed ", pilot_removed, " pilot account(s)")
    }
  }

  # if qa==TRUE, remove qa accounts
  if(qa) {
    rows_before_qa <- nrow(data_cleaned)

    if("assigning_districts" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_districts %in% qa_districts))
    }

    if("assigning_schools" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!(assigning_schools %in% qa_schools))
    }

    if("assessment_pid" %in% names(data_cleaned)) {
      data_cleaned <- data_cleaned %>%
        dplyr::filter(!str_detect(tolower(assessment_pid), "qa"))
    }

    qa_removed <- rows_before_qa - nrow(data_cleaned)
    if(verbose && qa_removed > 0) {
      message("Removed ", qa_removed, " QA account(s)")
    }
  }

  # if na==TRUE, remove accounts where assessment_pid==NA
  if(na==TRUE && "assessment_pid" %in% names(data_cleaned)) {
    rows_before_na <- nrow(data_cleaned)

    data_cleaned <- data_cleaned %>%
      dplyr::filter(!is.na(assessment_pid))

    na_removed <- rows_before_na - nrow(data_cleaned)
    if(verbose && na_removed > 0) {
      message("Removed ", na_removed, " account(s) with NA assessment_pid")
    }
  }

  # Final summary
  total_removed <- initial_rows - nrow(data_cleaned)

  if(verbose) {
    message("\n--- Account Removal Summary ---")
    message("Initial rows: ", initial_rows)
    message("Final rows: ", nrow(data_cleaned))
    message("Total removed: ", total_removed, " (",
            round(100 * total_removed / initial_rows, 2), "%)")

    if(total_removed == 0) {
      message("No accounts were removed.")
    }
  }

  # Warn if all data was removed
  if(nrow(data_cleaned) == 0 && initial_rows > 0 && verbose) {
    warning("All rows have been removed. Consider adjusting filter parameters.")
  }

  return(data_cleaned)
}

