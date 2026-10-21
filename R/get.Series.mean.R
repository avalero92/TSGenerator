#' Legacy mean time-series extractor
#'
#' Compatibility wrapper for the TSGenerator 1.x `get.Series.mean()` API.
#' New code should use [extract_ts()] with `fun = "mean"`.
#' @param pathRaster Directory containing TIFF rasters.
#' @param shapefile Polygon `sf`/`sfc` or `terra::SpatVector` object.
#' @param factorR Numeric scale divisor used by the legacy API.
#' @return A data frame with legacy columns `ID`, `Date`, and `Mean`.
#' @export
get.Series.mean <- function(pathRaster = NULL, shapefile = NULL, factorR = NULL) {
  .Deprecated("extract_ts")
  if (is.null(factorR) || !is.numeric(factorR) || length(factorR) != 1L || !is.finite(factorR) || factorR == 0) {
    stop("'factorR' must be one finite non-zero numeric value.", call. = FALSE)
  }
  z <- extract_ts(x = pathRaster, polygons = shapefile, fun = "mean", scale_factor = factorR)
  data.frame(ID = z$ID, Date = z$Date, Mean = z$Value, stringsAsFactors = FALSE)
}
