context("test_impcov_function.R")


test_adj <- impcov_adjust(hiv_prv_point = 0.4, hiv_cov_point = 0.8)
  
test_that("adjustment for imprefect HIV testing coverage is accurate", {
  expect_equal(round(test_adj, 7), 0.3878801)
})
