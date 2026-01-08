test_df <- data.frame(
  assigning_schools = c("[irNgj3c]", "irNgj3c"),
  age = c("", "6.7")
)

test_that("clean_strings converts empty strings to NA values and removes extra characters from the assigning organization variables", {
  expect_equal(clean_strings(test_df)[1, 1], "irNgj3c")
  expect_equal(clean_strings(test_df)[1, 2], NA_character_)
})
