test_that("ST quality definitions match documented codes", {
  z <- quality_info("ST")
  expect_equal(z$Code, 0:5)
  expect_equal(z$Class[z$Code == 5], "High")
  expect_true(all(z$AcceptDefault[z$Code %in% 3:5]))
})

test_that("VPP default policy keeps medium/high codes", {
  z <- quality_info("VPP")
  expect_equal(z$Code, 0:10)
  expect_true(all(z$AcceptDefault[z$Code %in% 7:10]))
  expect_false(any(z$AcceptDefault[z$Code %in% 0:6]))
})

test_that("quality classification preserves code semantics", {
  z <- classify_quality(c(0, 3, 5), "ST")
  expect_equal(z$Class, c("No data", "Low", "High"))
  expect_equal(z$Accepted, c(FALSE, TRUE, TRUE))
})

test_that("quality summary returns percentages", {
  z <- summarize_quality(c(3,3,4,5), "ST")
  expect_equal(sum(z$Freq), 4)
  expect_equal(sum(z$Percent), 100)
})

test_that("mask_quality does not resample mismatched geometry", {
  skip_if_not_installed("terra")
  x <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326", vals=1:4)
  q <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326", vals=c(2,3,4,5))
  y <- mask_quality(x,q,"ST")
  expect_true(is.na(terra::values(y)[1]))
  expect_equal(as.numeric(terra::values(y)[2:4]), 2:4)
  q2 <- terra::rast(nrows=4,ncols=4,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:4326", vals=5)
  expect_error(mask_quality(x,q2,"ST"), "identical geometry")
})


test_that("mask_quality supports explicit non-contiguous keep codes", {
  skip_if_not_installed("terra")
  x <- terra::rast(nrows=1,ncols=4,xmin=0,xmax=4,ymin=0,ymax=1,crs="EPSG:4326", vals=11:14)
  q <- terra::rast(nrows=1,ncols=4,xmin=0,xmax=4,ymin=0,ymax=1,crs="EPSG:4326", vals=c(1,3,4,5))
  y <- mask_quality(x, q, "ST", keep_codes=c(3,5))
  expect_equal(as.numeric(terra::values(y)), c(NA,12,NA,14))
})
