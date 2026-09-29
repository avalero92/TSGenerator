test_that("human-readable byte formatting is stable", {
  expect_equal(.tsg_human_size(0), "0 B")
  expect_equal(.tsg_human_size(1024), "1.00 KB")
  expect_equal(.tsg_human_size(1024^2), "1.00 MB")
})

test_that("metadata byte totals can be subset", {
  x <- data.frame(size = c(100, 200, NA_real_))
  expect_equal(.tsg_bytes(x), 300)
  expect_equal(.tsg_bytes(x, 2L), 200)
  expect_equal(.tsg_bytes(x, integer()), 0)
})

test_that("retry succeeds after a transient failure", {
  n <- 0L
  value <- .tsg_retry(function() {
    n <<- n + 1L
    if (n < 2L) stop("temporary")
    42L
  }, retries = 1L, backoff = 0)
  expect_equal(value, 42L)
  expect_equal(n, 2L)
})

test_that("offline WEkEO diagnostics return a structured table", {
  x <- check_wekeo(online = FALSE)
  expect_s3_class(x, "tsg_wekeo_check")
  expect_true(all(c("check", "ok", "detail") %in% names(x)))
  expect_true(all(c("R", "hdar", "jsonlite", "credentials_file") %in% x$check))
})
