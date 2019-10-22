context("test_plot_function.R")

data("ancrt")

ancrt_cleaned <- data_clean(ancrt)

test_that("descriptive_plot produces a plot", {
  plot <- descriptive_plot(ancrt_cleaned)
  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
})

prev_cov <- HIVprev_ipcw(ancrt_cleaned)
results <- impcov_adjust(prev_cov)

test_that("descriptive_plot produces a plot", {
  plot <- plot_rawadjusted(results, snu1 = "All", time.unit = "PERIOD", HIVraw = "TRUE", y.lim = 40)
})