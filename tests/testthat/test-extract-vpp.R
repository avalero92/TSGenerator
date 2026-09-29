test_that("VPP filename metadata is parsed", {
  m <- .tsg_parse_vpp_names(c("VPP_2020_S2_T30TXM-010m_V101_s1_SOSD.tif", "VPP_2020_S2_T30TXM-010m_V101_s2_MAXV.tif"))
  expect_equal(m$Year, c(2020L,2020L)); expect_equal(m$Season,c("s1","s2")); expect_equal(m$Product,c("SOSD","MAXV"))
})

test_that("YYDOY decoding respects leap years and invalid codes", {
  d <- as.Date(.tsg_decode_yydoy(c(20001,20060,20366,19365,0)), origin="1970-01-01")
  expect_equal(as.character(d[1:4]), c("2020-01-01","2020-02-29","2020-12-31","2019-12-31"))
  expect_true(is.na(d[5]))
})

test_that("extract_vpp applies product-aware scaling", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows=1,ncols=1,xmin=0,xmax=1,ymin=0,ymax=1,crs="EPSG:4326",nlyrs=3)
  terra::values(r) <- matrix(c(2500,1234,20060),nrow=1)
  names(r) <- c("VPP_2020_S2_T30TXM-010m_V101_s1_MAXV","VPP_2020_S2_T30TXM-010m_V101_s1_TPROD","VPP_2020_S2_T30TXM-010m_V101_s1_SOSD")
  p <- terra::vect("POLYGON ((0 0,1 0,1 1,0 1,0 0))",crs="EPSG:4326"); p$id <- "A"
  x <- extract_vpp(r,p,id_col="id",product=c("MAXV","TPROD","SOSD"),season="s1",year=2020)
  expect_equal(x$Value[1],0.25); expect_equal(x$Value[2],123.4); expect_equal(as.character(x$Date[3]),"2020-02-29")
})

test_that("VPP NoData values become missing", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows=1,ncols=1,xmin=0,xmax=1,ymin=0,ymax=1,crs="EPSG:4326"); terra::values(r) <- -32768
  p <- terra::vect("POLYGON ((0 0,1 0,1 1,0 1,0 0))",crs="EPSG:4326")
  x <- extract_vpp(r,p,product="MAXV",season="s1",year=2020)
  expect_true(is.na(x$Value))
})
