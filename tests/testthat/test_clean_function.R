context("test_clean_function.R")

ANCRTAdjust::ancrt

ancrt_cleaned <- ANCRTAdjust::data_clean(ancrt)
test_tot_status <- ancrt_cleaned$testneg_c + ancrt_cleaned$knownpos_c + ancrt_cleaned$testpos_c

testthat::test_that("data cleaning is internally consistent", {
  expect_equal(test_tot_status, ancrt_cleaned$n_status_c)
})
