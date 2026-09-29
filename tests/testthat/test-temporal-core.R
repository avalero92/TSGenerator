test_that("temporal_plan validates and diagnoses regular series", {
  x <- data.frame(ID = rep(c("A", "B"), each = 3),
                  Date = rep(as.Date("2020-01-01") + c(0, 10, 20), 2),
                  Value = c(1, NA, 3, 4, 5, 6))
  p <- temporal_plan(x, expected_step = 10)
  expect_s3_class(p, "tsg_temporal_plan")
  expect_equal(nrow(p$diagnostics), 2)
  expect_equal(p$diagnostics$n_missing, c(1, 0))
  expect_true(all(p$diagnostics$irregular_intervals == 0))
})

test_that("temporal_plan rejects duplicate ID-date rows by default", {
  x <- data.frame(ID = c("A", "A"), Date = as.Date(c("2020-01-01", "2020-01-01")), Value = 1:2)
  expect_error(temporal_plan(x), "Duplicate")
  expect_s3_class(temporal_plan(x, allow_duplicates = TRUE), "tsg_temporal_plan")
})

test_that("temporal_plan reports irregular temporal intervals", {
  x <- data.frame(ID = "A", Date = as.Date("2020-01-01") + c(0, 10, 25), Value = 1:3)
  p <- temporal_plan(x, expected_step = 10)
  expect_equal(p$diagnostics$irregular_intervals, 1)
})
