test_that("date parser extracts supported filename dates", {
  expect_equal(
    TSGenerator::extract_dates_from_tiff_files(c("x_2024-01-02.tif", "x_20240112.tif")),
    c("2024-01-02", "20240112")
  )
})

test_that("date parser rejects filenames without dates", {
  expect_error(TSGenerator::extract_dates_from_tiff_files("no_date.tif"), "Could not extract")
})

test_that("legacy count_missing points to the 2.0 temporal core", {
  d <- data.frame(DOY = c(50, 100), Year = c(2020, 2020), ID = c(1, 1), NDVI = c(NA, 0.4))
  expect_error(
    suppressWarnings(TSGenerator::count_missing(d, sos = 40, maxd = 80, eos = 120)),
    "summarize_missingness"
  )
})
