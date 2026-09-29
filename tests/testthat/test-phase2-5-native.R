test_that("active package no longer declares reticulate", {
  imports <- packageDescription("TSGenerator", fields = c("Imports", "Suggests"))
  expect_false(any(grepl("reticulate", imports, fixed = TRUE)))
})

test_that("legacy ST/VPP wrappers remain exported functions", {
  expect_true(is.function(Download.STPPI))
  expect_true(is.function(Download.HRVPP))
})

test_that("legacy VI downloader gives migration error", {
  expect_error(Download.VI(), "no longer operational")
})
