#' Legacy VPP extractor
#'
#' The TSGenerator 1.x VPP extractor applied one user-supplied scale factor to
#' every VPP parameter. That behaviour is scientifically incompatible with the
#' product-aware TSGenerator 2.0 VPP engine and is therefore not emulated.
#' @param pathRaster Legacy raster directory.
#' @param shapefile Legacy polygon object.
#' @param factorR Legacy scale factor.
#' @return No return value. This retired function always signals an error
#'   explaining the migration to `extract_vpp()`.
#' @export
get.Series.VPP <- function(pathRaster = NULL, shapefile = NULL, factorR = NULL) {
  stop("get.Series.VPP() is retired from the active 2.0 geospatial core because its single factorR scaling is not valid across VPP products. Use extract_vpp(), which applies product-specific decoding, scaling and NoData rules.", call. = FALSE)
}
