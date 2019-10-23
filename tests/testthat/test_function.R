context("test_plot_function.R")

ANCRTAdjust::ancrt

ancrt_cleaned <- ANCRTAdjust::data_clean(ancrt)

testthat::test_that("descriptive_plot produces a plot", {
  plot <- ANCRTAdjust::descriptive_plot(ancrt_cleaned)
  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
})

prv_cov <- ANCRTAdjust::hiv_prv_ipcw(ancrt_cleaned, by_snu1 = FALSE, by_period = TRUE, by_year = FALSE)
results <- ANCRTAdjust::impcov_adjust(data = prv_cov)

testthat::test_that("plot_rawadjusted produces a plot", {
  plot <- ANCRTAdjust::plot_rawadjusted(data = results, snu1 = "all", time_unit = "period", hiv_raw = TRUE, y_lim = 40)
  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
})