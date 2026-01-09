test_df <- data.frame(
  assessment_pid = c("123", "456", NA, "789", "123"),
  roarScore = c(45, 32, 34, 10, 45)
)

test_that("remove_duplicates() removes rows that contain all the same values across every column", {
  expect_equal(length(remove_duplicates(test_df)$assessment_pid), 4)
  expect_equal(remove_duplicates(test_df)$assessment_pid, c("123", "456", NA, "789"))
})
