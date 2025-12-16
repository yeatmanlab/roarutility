test_that("roar.read.csv removes opt-outs from a ROAR data csv", {
  expect_equal(dim(roar.read.csv("all_core_runs_2025-05-27.csv",
                             "~/Documents/GitHub/roar-technical-manual/data",
                             "https://drive.google.com/file/d/11gYLqU5xT-NMDxWXGQj8WfZ8AVA_lFT9/view?usp=drive_link"))[1],
               111777)
})
