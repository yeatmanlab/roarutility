test_df <- data.frame(
  user.grade = c("2", "1", "01", "2nd", "k", "Kindergarten", "1", "09")
)

test_that("standardize_grade cleans grade to uniform values", {
  expect_equal(standardize_grade(test_df, "user.grade")[,1], c("2", "1", "1",
                                                              "2",
                                                              "Kindergarten",
                                                              "Kindergarten",
                                                              "1", "9"))
})
