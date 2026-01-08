test_df <- data.frame(
  firstname = c("Jane", "John", NA, "Kelly"),
  lastname = c("Doe", NA, NA, "Smith"),
  middlename = c(NA, NA, NA, NA)
)

test_that("remove_empty_cols removes columns within a dataframe if they contain all NA values", {
  expect_equal(names(remove_empty_cols(test_df)), c("firstname", "lastname"))
})
