context("test_datasets")

data(ancrt)

test_that("survey dataset has correct factor levels", {
  expect_equal(levels(ancrt$faciluid), levels(as.factor(paste("F_", seq(1, 1000), sep = ""))))
})

