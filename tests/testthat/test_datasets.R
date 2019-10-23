context("test_datasets")

ANCRTAdjust::ancrt

testthat::test_that("survey dataset has correct factor levels", {
  expect_equal(levels(ancrt$faciluid), levels(as.factor(paste("F_", seq(1, 1000), sep = ""))))
})

