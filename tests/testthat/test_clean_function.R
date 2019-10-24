context("test_clean_function.R")

ANCRTAdjust::ancrt

ancrt_checked <- check_data(ancrt, faciluid = "faciluid", time = "time", n_clients = 'n_clients', 
                    n_status = "n_status", knownpos = "knownpos", testpos = "testpos", 
                    testneg = "testneg", snu1 = "snu1", year = "year")

testthat::test_that("checking data function is working as intended", {
  expect_equal(nrow(ancrt), nrow(ancrt_checked))
  expect_equal(nrow(ancrt), ncol(ancrt_checked))
})


ancrt_cleaned <- ANCRTAdjust::data_clean(ancrt)
test_tot_status <- ancrt_cleaned$testneg_c + ancrt_cleaned$knownpos_c + ancrt_cleaned$testpos_c

testthat::test_that("data cleaning is internally consistent", {
  expect_equal(test_tot_status, ancrt_cleaned$n_status_c)
})
