context("test_impcov_function.R")


test_adj <- ANCRTAdjust::impcov_adjust(hiv_prv_point = 40, hiv_cov_point = 80)
  
testthat::test_that("adjustment for imprefect HIV testing coverage is accurate", {
  expect_equal(round(test_adj, 5), 38.78801)
})
