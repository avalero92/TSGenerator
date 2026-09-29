test_that("legacy temporal quality interfaces fail with migration guidance", {
  d <- data.frame(ID = "A", Date = as.Date("2020-01-01"), Value = 1)
  expect_error(suppressWarnings(quality.Series(d, "ID", "Date", "Value")), "assess_ts_quality")
  expect_error(suppressWarnings(general.Quality(data.frame(x=1), "x", "x")), "assess_ts_quality")
})

test_that("legacy phenology missing-count interface points to 2.0 core", {
  d <- data.frame(ID="A", Year=2020, DOY=1, NDVI=NA_real_)
  expect_error(suppressWarnings(count_missing(d, 50, 150, 300)), "summarize_missingness")
})

test_that("new temporal core functions remain exported", {
  expect_true(is.function(temporal_plan))
  expect_true(is.function(summarize_missingness))
  expect_true(is.function(assess_ts_quality))
  expect_true(is.function(impute_ts))
  expect_true(is.function(model_missingness))
  expect_true(is.function(plot_missingness))
})
