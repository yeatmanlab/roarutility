library(purrr)
library(dplyr)

test_df <- data.frame(
  age_months = c(75, 83, 99, 200),
  user_grade = c(NA, "2", "2", "10"),
  time_started = c("2025-03-12", "2024-02-17", "2026-01-09", "2025-04-19")
)

clean_df <- test_df %>% mutate(user_grade = case_when(
  time_started < as.Date("2024-07-31") ~ map_chr(age_months, estimate_grade),
  TRUE ~ user_grade))

clean_df <- clean_df %>% mutate(user_grade = case_when(
  is.na(user_grade) ~ map_chr(age_months, estimate_grade),
  TRUE ~ user_grade))

test_that("estimate_grade uses age in months to determine missing grades and grades logged before 24-25 school year", {
  expect_equal(clean_df[2,2], "1")
  expect_equal(clean_df[1,2], "1")
})
