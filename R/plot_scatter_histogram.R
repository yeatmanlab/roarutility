#' Create scatter plot with marginal histograms
#'
#' @description
#' Uses grade, proportion correct, and median response time to create a
#' marginal distribution plot. The main plot is a scatter plot showing
#' the relationship between proportion correct and median response time.
#' The marginal histograms are bar charts along the top and right edges of the
#' main plot that show the distribution of each variable independently.
#'
#'
#' @param df The dataframe that contains grade, proportion correct,
#' and median response time.
#' @param grade A string value to indicate the name of the variable representing
#' the student's grade (default: "user_grade_at_run")
#' @param prop_correct A string value to indicate the name of the variable
#' representing the proportion correct for each student's run (default:
#' "prop_correct")
#' @param median_rt A string value to indicate the name of the variable
#' representing the median response time the student's run (default:
#' "median_rt")
#' @param verbose A Boolean value, TRUE (default) or FALSE, that controls whether
#' informational messages and warnings are displayed
#'
#' @returns A marginal distribution plot showing the relationship between a ROAR
#' assessment proportion correct and the median response time
#' @export
#'
#' @importFrom ggplot2 aes
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggExtra ggMarginal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr mutate
plot_scatter_histogram <- function(df,
                                   grade = "user_grade_at_run",
                                   prop_correct = "prop_correct",
                                   median_rt = "median_rt",
                                   verbose = TRUE) {

  # Input validation: Check if df is a dataframe
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe")
  }

  # Check if dataframe is empty
  if (nrow(df) == 0) {
    stop("Input dataframe is empty (0 rows)")
  }

  # Check for expected columns
  expected_cols <- c(grade, prop_correct, median_rt)
  missing_cols <- setdiff(expected_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("The following columns are not present in the dataframe: ",
         paste(missing_cols, collapse = ", "))
  }

  # Rename columns to standardized names for internal use
  df_work <- df
  names(df_work)[names(df_work) == grade] <- "grade_col"
  names(df_work)[names(df_work) == prop_correct] <- "prop_correct_col"
  names(df_work)[names(df_work) == median_rt] <- "median_rt_col"

  # Check for all NA values
  if (all(is.na(df_work$grade_col))) {
    stop("All values in '", grade, "' are NA")
  }

  if (all(is.na(df_work$prop_correct_col))) {
    stop("All values in '", prop_correct, "' are NA")
  }

  if (all(is.na(df_work$median_rt_col))) {
    stop("All values in '", median_rt, "' are NA")
  }

  # Validate prop_correct values are between 0 and 1
  valid_prop <- df_work$prop_correct_col[!is.na(df_work$prop_correct_col)]
  if (any(valid_prop < 0 | valid_prop > 1)) {
    if (verbose) {
      warning("Some values in '", prop_correct, "' are outside the range [0, 1]. ",
              "Expected proportion values between 0 and 1.")
    }
  }

  # Validate median_rt values are positive
  valid_rt <- df_work$median_rt_col[!is.na(df_work$median_rt_col)]
  if (any(valid_rt < 0)) {
    if (verbose) {
      warning("Some values in '", median_rt, "' are negative. ",
              "Response times should be positive values.")
    }
  }

  # Count and warn about NA values
  na_count_grade <- sum(is.na(df_work$grade_col))
  na_count_prop <- sum(is.na(df_work$prop_correct_col))
  na_count_rt <- sum(is.na(df_work$median_rt_col))

  if (verbose && (na_count_grade > 0 || na_count_prop > 0 || na_count_rt > 0)) {
    message("NA values found - ",
            grade, ": ", na_count_grade, ", ",
            prop_correct, ": ", na_count_prop, ", ",
            median_rt, ": ", na_count_rt)
  }

  # Filter out rows with NA in grade
  df.cleaned <- df_work %>%
    dplyr::filter(!is.na(.data$grade_col))

  # Check if any data remains after filtering
  if (nrow(df.cleaned) == 0) {
    stop("No data remains after removing NA values from '", grade, "'")
  }

  if (verbose) {
    message("Plotting ", nrow(df.cleaned), " observations (",
            nrow(df) - nrow(df.cleaned), " removed due to NA in grade)")
  }

  # Convert grade to factor with expected levels
  grade_levels <- c("Kindergarten", "1", "2", "3", "4", "5", "6",
                    "7", "8", "9", "10", "11", "12")

  # Check if grade values match expected levels
  unique_grades <- unique(df.cleaned$grade_col[!is.na(df.cleaned$grade_col)])
  unexpected_grades <- setdiff(as.character(unique_grades), grade_levels)

  if (length(unexpected_grades) > 0 && verbose) {
    warning("Unexpected grade values found: ",
            paste(unexpected_grades, collapse = ", "),
            ". Expected values: ", paste(grade_levels, collapse = ", "))
  }

  df.cleaned <- df.cleaned %>%
    dplyr::mutate(
      grade_col = factor(.data$grade_col, levels = grade_levels)
    )

  # Count how many factor levels actually have data
  n_grades_with_data <- sum(table(df.cleaned$grade_col) > 0)

  if (verbose && n_grades_with_data < 13) {
    message("Data present for ", n_grades_with_data, " out of 13 grade levels")
  }

  # Create the scatter plot
  scatter <- df.cleaned %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$prop_correct_col,
                                 y = .data$median_rt_col,
                                 col = .data$grade_col)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::labs(x = "Proportion Correct",
                  y = "Median Response Time (milliseconds)",
                  col = "Grade") +
    ggplot2::scale_color_manual(
      values = grDevices::colorRampPalette(c('dodgerblue1',
                                             'firebrick1',
                                             'goldenrod1'))(13),
      drop = FALSE  # Show all levels even if no data
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Avenir", size = 14),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      aspect.ratio = 1
    )

  # Create marginal histogram
  scatter_histogram <- ggExtra::ggMarginal(scatter, type = "histogram")

  return(scatter_histogram)
}
