test_that("date validation supports compact HR-VPP dates", {
  d <- .tsg_validate_dates(c("20200101", "2020-01-11"), 2)
  expect_s3_class(d, "Date")
  expect_equal(as.character(d), c("2020-01-01", "2020-01-11"))
})

test_that("date validation enforces one date per layer", {
  expect_error(.tsg_validate_dates("20200101", 2), "exactly 2")
  expect_error(.tsg_validate_dates("not-a-date", 1), "cannot be converted")
  expect_error(.tsg_validate_dates("2020-99-99", 1), "cannot be converted")
})

test_that("extract_ts returns tidy polygon x layer output", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                   crs = "EPSG:4326", nlyrs = 2)
  terra::values(r) <- cbind(1:4, 11:14)
  names(r) <- c("PPI_20200101", "PPI_20200111")
  p <- terra::vect("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))", crs = "EPSG:4326")
  p$parcel <- "A"
  x <- extract_ts(r, p, id_col = "parcel", fun = "mean", dates = c("20200101", "20200111"))
  expect_s3_class(x, "tsg_time_series")
  expect_equal(names(x), c("ID", "Date", "Layer", "Value"))
  expect_equal(x$ID, c("A", "A"))
  expect_equal(x$Value, c(2.5, 12.5))
})

test_that("extract_ts applies scaling after extraction", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                   crs = "EPSG:4326")
  terra::values(r) <- 2500
  names(r) <- "PPI_20200101"
  p <- terra::vect("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", crs = "EPSG:4326")
  x <- extract_ts(r, p, fun = "median", dates = "20200101", scale_factor = 10000)
  expect_equal(x$Value, 0.25)
})


test_that("extract_ts supports duplicate HR-VPP band descriptions", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                   crs = "EPSG:4326", nlyrs = 2)
  terra::values(r) <- cbind(c(0.01, 0.03, 0.04, 0.0488),
                            c(0.02, 0.04, 0.05, 0.0472))
  names(r) <- rep("Plant Phenology Index obtained via timesat fitting", 2)
  p <- terra::vect("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))", crs = "EPSG:4326")

  x <- extract_ts(
    r, p, fun = "median",
    dates = c("20180301", "20180311")
  )

  expect_s3_class(x, "tsg_time_series")
  expect_equal(nrow(x), 2L)
  expect_equal(as.character(x$Date), c("2018-03-01", "2018-03-11"))
  expect_equal(length(x$Value), 2L)
  expect_false(anyNA(x$Value))
  expect_equal(length(unique(x$Layer)), 1L)
})
