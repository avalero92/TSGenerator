test_that("live temporal fields accept current and transition schemas", {
  expect_equal(names(c(start = "start", end = "end")), c("start", "end"))
  q1 <- TSGenerator:::.st_build_query(
    "PPI", as.Date("2020-01-01"), as.Date("2020-01-10"),
    "30TXM", NULL, "S2A, S2B", 10, NULL,
    temporal_fields = c(start = "startdate", end = "enddate")
  )$list
  expect_true(all(c("startdate", "enddate") %in% names(q1)))
  expect_false(any(c("start", "end") %in% names(q1)))

  q2 <- TSGenerator:::.vpp_build_query(
    "TPROD", "s1", as.Date("2018-01-01"), as.Date("2018-12-31"),
    "30TXR", NULL, "S2A, S2B", NULL,
    temporal_fields = c(start = "startdate", end = "enddate")
  )$list
  expect_true(all(c("startdate", "enddate") %in% names(q2)))
})

test_that("live integration test is opt-in", {
  skip_if(Sys.getenv("TSGENERATOR_LIVE_WEKEO") != "true", "Set TSGENERATOR_LIVE_WEKEO=true for live WEkEO integration")
  z <- check_wekeo_integration(download = FALSE, quiet = TRUE)
  expect_s3_class(z, "tsg_wekeo_integration")
  expect_true(z$ok)
})
