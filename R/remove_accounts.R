#' Remove test, demo, pilot, QA, or NA accounts
#'
#' @description
#' Removes all indicated accounts from the dataframe. Reseearchers can read in
#'  data and indicate which or all of the following types of accounts they would
#'  like to remove from the dataframe. The function defaults to removing test,
#'  demo, pilot, and QA accounts and defaults to not removing NA assessment_pid.
#'   The function runs through the organization IDs
#'   (i.e., assigning_districts, etc.). It also uses string detection to
#'   determine if there are any "test", "pilot", "qa", or "demo" strings
#'   within the assessment_pid column. Finally, it runs through to determine
#'   if there are test or demo using the variables is_test_data and is_demo_data
#'    (if these accounts were chosen to be removed). If selected, the function
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
#'
#' @returns A dataframe with the indicated remaining account types
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
remove_accounts <- function(df, test=TRUE, demo=TRUE, pilot=TRUE, qa=TRUE, na=FALSE){

  # set new dataframe
  data_cleaned <- df

  # Declare variables to avoid notes on check()
  is_test_data <- is_demo_data <- assessment_pid <- assigning_districts <-
    assigning_groups <- assigning_schools <- NULL

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
  if(test && "is_test_data" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(is_test_data == "false")
  }

  if(test && "assigning_districts" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_districts %in% test_districts))
  }

  if(test && "assigning_schools" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_schools %in% test_schools))
  }

  if(test && "assigning_groups" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_groups %in% test_groups))
  }

  if(test && "assessment_pid" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!str_detect(tolower(assessment_pid), "test"))
  }


  # if demo==TRUE, remove demo accounts
  if(demo && "is_demo_data" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(is_demo_data == "false")
  }

  if(demo && "assigning_districts" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_districts %in% demo_districts))
  }

  if(demo && "assigning_schools" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_schools %in% demo_schools))
  }

  if(demo && "assessment_pid" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!str_detect(tolower(assessment_pid), "demo"))
  }

  # if pilot==TRUE, remove pilot accounts
  if(pilot && "assigning_districts" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_districts %in% pilot_districts))
  }

  if(pilot && "assigning_schools" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_schools %in% pilot_schools))
  }

  if(pilot && "assessment_pid" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!str_detect(tolower(assessment_pid), "pilot"))
  }

  # if qa==TRUE, remove qa accounts
  if(qa && "assigning_districts" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_districts %in% qa_districts))
  }

  if(qa && "assigning_schools" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!(assigning_schools %in% qa_schools))
  }

  if(qa && "assessment_pid" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!str_detect(tolower(assessment_pid), "qa"))
  }

  # if na==TRUE, remove accounts where assessment_pid==NA
  if(na==TRUE && "assessment_pid" %in% names(data_cleaned)) {
    data_cleaned <- data_cleaned %>%
      dplyr::filter(!is.na(assessment_pid))
  }

  return(data_cleaned)
}

