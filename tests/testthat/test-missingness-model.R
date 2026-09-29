test_that("model_missingness treats year as factor by default", {
  skip_if_not_installed("mgcv")
  d <- expand.grid(Year = 2020:2021, DOY = seq(1, 101, 10), ID = 1:4)
  d$Value <- 1
  d$Value[c(2, 7, 20)] <- NA
  z <- model_missingness(d, value_col = "Value")
  expect_s3_class(z, "tsg_missingness_model")
  expect_equal(z$year_effect, "factor")
  expect_true(all(z$data$Predicted >= 0 & z$data$Predicted <= 1))
  expect_true("YearFactor" %in% names(z$data))
})

test_that("plot_missingness returns ggplot", {
  skip_if_not_installed("mgcv")
  d <- expand.grid(Year = 2020:2021, DOY = seq(1, 101, 10), ID = 1:3)
  d$Value <- seq_len(nrow(d)); d$Value[c(3, 9)] <- NA
  z <- model_missingness(d)
  expect_s3_class(plot_missingness(z), "ggplot")
})

test_that("invalid DOY is rejected", {
  d <- data.frame(Year = 2020, DOY = 400, Value = NA_real_)
  expect_error(model_missingness(d), "DOY")
})
