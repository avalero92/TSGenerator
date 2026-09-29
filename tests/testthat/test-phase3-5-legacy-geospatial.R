test_that("legacy time-series wrappers delegate to terra core", {
  expect_true(is.function(get.Series.mean))
  expect_true(is.function(get.Series.median))
})

test_that("retired VI geospatial functions fail with migration guidance", {
  expect_error(get.Stack(), "discontinued VI/QFLAG2")
  expect_error(get.Clean.IV(), "discontinued VI/QFLAG2")
  expect_error(QFLAG2.Mask(), "discontinued VI QFLAG2")
  expect_error(renames.image.IV(), "discontinued VI")
})

test_that("legacy VPP scaling API is explicitly retired", {
  expect_error(get.Series.VPP(), "Use extract_vpp")
  expect_error(renames.image.VPP(), "extract_vpp")
})
