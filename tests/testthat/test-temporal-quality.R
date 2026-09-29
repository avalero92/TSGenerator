test_that("missingness separates absent dates from explicit NA", {
  x <- data.frame(ID="A", Date=as.Date(c("2020-01-01","2020-01-11","2020-01-31")), Value=c(1,NA,4))
  z <- summarize_missingness(x, expected_step=10)
  expect_equal(z$n_expected, 4)
  expect_equal(z$n_observed_dates, 3)
  expect_equal(z$n_absent_dates, 1)
  expect_equal(z$n_value_na, 1)
  expect_equal(z$n_usable, 2)
  expect_equal(z$value_completeness, 0.5)
})

test_that("quality uses expected temporal completeness", {
  x <- data.frame(ID="A", Date=as.Date(c("2020-01-01","2020-01-11","2020-01-21","2020-01-31")), Value=c(1,2,3,4))
  z <- assess_ts_quality(x, expected_step=10)
  expect_equal(as.character(z$Quality), "High")
})

test_that("quality thresholds are configurable", {
  x <- data.frame(ID="A", Date=as.Date(c("2020-01-01","2020-01-11","2020-01-31")), Value=c(1,2,3))
  z <- assess_ts_quality(x, expected_step=10, thresholds=c(high=.95,medium=.70,low=.40))
  expect_equal(as.character(z$Quality), "Medium")
})

test_that("windowed assessment returns consecutive windows", {
  x <- data.frame(ID="A", Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-06-29"), by="10 days"), Value=1)
  z <- assess_ts_quality(x, expected_step=10, window_days=91)
  expect_true(nrow(z) >= 2)
  expect_true(all(c("window","Quality","value_completeness") %in% names(z)))
})

test_that("duplicate ID-date records are rejected", {
  x <- data.frame(ID=c("A","A"), Date=as.Date(c("2020-01-01","2020-01-01")), Value=c(1,2))
  expect_error(summarize_missingness(x, expected_step=10), "Duplicate")
})
