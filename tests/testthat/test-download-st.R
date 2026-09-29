test_that("ST product validation is strict", {
  expect_equal(.st_validate_product("ppi"), "PPI")
  expect_equal(.st_validate_product(c("PPI", "qflag")), c("PPI", "QFLAG"))
  expect_error(.st_validate_product("NDVI"), "Unsupported")
})

test_that("ST tile validation normalizes MGRS tile IDs", {
  expect_equal(.st_validate_tile("T30TXM"), "30TXM")
  expect_equal(.st_validate_tile("30txm"), "30TXM")
  expect_error(.st_validate_tile("30TX"), "MGRS")
})

test_that("ST bbox validation catches invalid geographic bounds", {
  expect_equal(.st_validate_bbox(c(-1, 40, 0, 41)), c(-1, 40, 0, 41))
  expect_error(.st_validate_bbox(c(0, 40, -1, 41)), "EPSG:4326")
  expect_error(.st_validate_bbox(c(-181, 40, 0, 41)), "EPSG:4326")
})

test_that("long ST periods are split into maximum 31-day windows", {
  w <- .st_date_windows(as.Date("2020-01-01"), as.Date("2020-03-15"))
  expect_equal(nrow(w), 3L)
  expect_true(all(as.integer(w$end - w$start) + 1L <= 31L))
  expect_equal(w$start[1], as.Date("2020-01-01"))
  expect_equal(w$end[nrow(w)], as.Date("2020-03-15"))
})

test_that("ST query uses current HDA parameter names", {
  skip_if_not_installed("jsonlite")
  q <- .st_build_query("PPI", as.Date("2020-04-01"), as.Date("2020-04-30"),
                       "30TXM", c(-1, 40, 0, 41), "S2A, S2B", 10, NULL)
  expect_equal(q$list$dataset_id, "EO:EEA:DAT:CLMS_HRVPP_ST")
  expect_equal(q$list$productType, "PPI")
  expect_equal(q$list$tileId, "30TXM")
  expect_equal(q$list$resolution, "10")
  expect_match(q$json, '"productType":"PPI"', fixed = TRUE)
})
