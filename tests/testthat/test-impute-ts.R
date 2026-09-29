test_that("linear imputation preserves original values and provenance", {
  d <- data.frame(ID = "A", Date = as.Date("2020-01-01") + c(0,10,20), Value = c(1, NA, 3))
  z <- impute_ts(d, method = "linear")
  expect_equal(z$Value, c(1, NA, 3))
  expect_equal(z$Value_imputed, c(1, 2, 3))
  expect_equal(z$.WasImputed, c(FALSE, TRUE, FALSE))
  expect_s3_class(z, "tsg_imputed_series")
})

test_that("ST imputation is blocked unless explicitly enabled", {
  d <- data.frame(ID = "A", Date = as.Date("2020-01-01") + c(0,10,20), Value = c(1, NA, 3))
  expect_error(impute_ts(d, method = "linear", series_type = "st"), "blocked by default")
  expect_s3_class(impute_ts(d, method = "linear", series_type = "st", allow_processed = TRUE), "tsg_imputed_series")
})

test_that("missing expected dates can be inserted explicitly", {
  d <- data.frame(ID = "A", Date = as.Date(c("2020-01-01", "2020-01-21")), Value = c(1,3))
  z <- impute_ts(d, method = "linear", complete_grid = TRUE, expected_step = 10)
  expect_equal(nrow(z), 3)
  expect_true(z$.InsertedDate[2])
  expect_equal(z$Value_imputed[2], 2)
})

test_that("long missing runs can be protected", {
  d <- data.frame(ID = "A", Date = as.Date("2020-01-01") + 0:4, Value = c(1,NA,NA,NA,5))
  z <- impute_ts(d, method = "linear", max_gap = 2)
  expect_true(all(is.na(z$Value_imputed[2:4])))
})

test_that("edge gaps are not imputed by default", {
  d <- data.frame(ID = "A", Date = as.Date("2020-01-01") + 0:3, Value = c(NA,2,3,NA))
  z <- impute_ts(d, method = "linear")
  expect_true(is.na(z$Value_imputed[1]))
  expect_true(is.na(z$Value_imputed[4]))
})
