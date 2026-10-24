# 05.B.3-E: regression for both supported GDAL scale conventions.
test_that("VPP MAXV and TPROD scaling supports raw and GDAL-scaled files", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  for (product in c("MAXV", "TPROD")) {
    spec <- .tsg_vpp_specs()
    spec <- spec[spec$Product == product, , drop = FALSE]
    factor <- spec$ScaleFactor
    raw <- c(1000, 2000, 3000, 4000)
    expected <- raw / factor
    r <- terra::rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2)
    terra::values(r) <- raw
    td <- tempfile("vpp-scale-"); dir.create(td)
    unscaled <- file.path(td, "unscaled.tif")
    scaled <- file.path(td, "scaled.tif")
    terra::writeRaster(r, unscaled, overwrite=TRUE, datatype="INT2S")
    # GDAL -a_scale/-a_offset must be applied to a file, not only to an
    # in-memory SpatRaster (writeRaster may drop scoff metadata).
    sf::gdal_utils("translate", unscaled, scaled,
      options=c("-of", "GTiff", "-a_scale", format(1/factor, scientific=FALSE),
                "-a_offset", "0"), quiet=TRUE)
    a <- terra::rast(unscaled); b <- terra::rast(scaled)
    expect_equal(as.numeric(terra::scoff(a)[1,]), c(1,0))
    expect_equal(as.numeric(terra::scoff(b)[1,]), c(1/factor,0), tolerance=1e-10)
    expect_equal(as.numeric(terra::values(b)), expected, tolerance=1e-8)
    expect_equal(as.numeric(terra::values(.tsg_prepare_vpp_layer(a,spec))), expected, tolerance=1e-8)
    expect_equal(as.numeric(terra::values(.tsg_prepare_vpp_layer(b,spec))), expected, tolerance=1e-8)
    unlink(td, recursive=TRUE)
  }
})

test_that("Unexpected VPP scale/offset is rejected", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  spec <- .tsg_vpp_specs()
  spec <- spec[spec$Product == "MAXV", , drop = FALSE]
  td <- tempfile("vpp-invalid-scale-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  raw_path <- file.path(td, "raw.tif")
  bad_path <- file.path(td, "invalid-scale.tif")
  r <- terra::rast(nrows = 1, ncols = 1)
  terra::values(r) <- 1000
  terra::writeRaster(r, raw_path, overwrite = TRUE, datatype = "INT2S")
  sf::gdal_utils("translate", raw_path, bad_path,
    options = c("-of", "GTiff", "-a_scale", "0.5", "-a_offset", "0"),
    quiet = TRUE)
  bad <- terra::rast(bad_path)
  # Check the fixture before evaluating the software contract.
  expect_equal(as.numeric(terra::scoff(bad)[1, ]), c(0.5, 0), tolerance = 1e-10)
  expect_error(.tsg_prepare_vpp_layer(bad, spec),
               "Unsupported VPP scale/offset")
})
