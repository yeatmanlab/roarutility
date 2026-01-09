test_df <- read.csv("~/Documents/ROAR Primary Research Data/Organizations/organization_key_2026-01-07.csv")


test_that("remove_accounts removes all indicated accounts (test, demo, pilot, qa, and na)", {
  expect_equal(dim(remove_accounts(test_df))[1], 2678)
})
