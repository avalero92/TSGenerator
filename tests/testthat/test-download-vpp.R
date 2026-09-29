test_that("VPP products are validated and normalized", {
  expect_equal(TSGenerator:::.vpp_validate_product(c("sosd", "EOSD")), c("SOSD", "EOSD"))
  expect_equal(TSGenerator:::.vpp_validate_product("all"), TSGenerator:::.VPP_PRODUCTS)
  expect_error(TSGenerator:::.vpp_validate_product("NDVI"), "Unsupported VPP")
})

test_that("VPP seasons are validated", {
  expect_equal(TSGenerator:::.vpp_validate_season(c("S1", "s2")), c("s1", "s2"))
  expect_null(TSGenerator:::.vpp_validate_season(NULL))
  expect_error(TSGenerator:::.vpp_validate_season("s3"), "Unsupported VPP season")
})

test_that("VPP tile identifiers are normalized", {
  expect_equal(TSGenerator:::.vpp_validate_tile("T30TXM"), "30TXM")
  expect_equal(TSGenerator:::.vpp_validate_tile("30txm"), "30TXM")
  expect_error(TSGenerator:::.vpp_validate_tile("30TX"), "MGRS")
})

test_that("VPP query uses current HDA field names", {
  q <- TSGenerator:::.vpp_build_query(
    product = "SOSD", season = "s1",
    start = as.Date("2020-01-01"), end = as.Date("2020-12-31"),
    tile_id = "30TXM", bbox = NULL, platform = "S2A, S2B",
    product_version = NULL
  )$list
  expect_equal(q$dataset_id, "EO:EEA:DAT:CLMS_HRVPP_VPP")
  expect_equal(q$productType, "SOSD")
  expect_equal(q$productGroupId, "s1")
  expect_equal(q$tileId, "30TXM")
  expect_equal(q$platformSerialIdentifier, "S2A, S2B")
  expect_true(all(c("start", "end", "itemsPerPage", "startIndex") %in% names(q)))
})

test_that("VPP query can omit season and spatial filters", {
  q <- TSGenerator:::.vpp_build_query(
    product = "TPROD", season = NULL,
    start = as.Date("2018-01-01"), end = as.Date("2018-12-31"),
    tile_id = NULL, bbox = NULL, platform = NULL, product_version = NULL
  )$list
  expect_false("productGroupId" %in% names(q))
  expect_false("tileId" %in% names(q))
  expect_false("bbox" %in% names(q))
})

test_that("VPP bbox and dates are validated", {
  expect_equal(TSGenerator:::.vpp_validate_bbox(c(-1, 40, 0, 41)), c(-1, 40, 0, 41))
  expect_error(TSGenerator:::.vpp_validate_bbox(c(0, 40, -1, 41)), "EPSG:4326")
  expect_error(TSGenerator:::.vpp_validate_dates("2021-01-01", "2020-01-01"), "earlier")
})
