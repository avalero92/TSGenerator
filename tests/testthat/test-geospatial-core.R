test_that("summary functions are validated", {
  expect_true(is.function(.tsg_match_summary_fun("mean")))
  expect_true(is.function(.tsg_match_summary_fun("median")))
  expect_error(.tsg_match_summary_fun("mode"), "Unsupported")
})

test_that("scaling is explicit and validated", {
  expect_equal(.tsg_validate_scale(10000, 0)$scale_factor, 10000)
  expect_error(.tsg_validate_scale(0, 0), "non-zero")
  expect_error(.tsg_validate_scale(1, NA_real_), "finite")
})

test_that("TIFF file discovery rejects unsupported files", {
  f <- tempfile(fileext = ".csv")
  writeLines("x", f)
  expect_error(.tsg_list_rasters(f), "TIFF")
})

test_that("geospatial plan aligns CRS and preserves explicit IDs", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326")
  p <- terra::as.polygons(terra::ext(0, 2, 0, 2), crs = "EPSG:4326")
  p$id <- "A"
  z <- geospatial_plan(r, p, id_col = "id")
  expect_s3_class(z, "tsg_geospatial_plan")
  expect_equal(z$n_features, 1)
  expect_equal(z$ids, "A")
  expect_true(terra::same.crs(z$raster, z$polygons))
})
