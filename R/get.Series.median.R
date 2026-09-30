#' Legacy median time-series extractor
#'
#' Compatibility wrapper for the TSGenerator 1.x `get.Series.median()` API.
#' New code should use [extract_ts()] with `fun = "median"`.
#' @param pathRaster Directory containing TIFF rasters.
#' @param shapefile Polygon `sf`/`sfc` or `terra::SpatVector` object.
#' @param factorR Numeric scale divisor used by the legacy API.
#' @return A data frame with legacy columns `ID`, `Date`, and `Mean`. The legacy
#'   column name `Mean` is preserved even though the statistic is the median.
#' @export
get.Series.median <- function(pathRaster = NULL, shapefile = NULL, factorR = NULL) {
  .Deprecated("extract_ts")
  if (is.null(factorR) || !is.numeric(factorR) || length(factorR) != 1L || !is.finite(factorR) || factorR == 0) {
    stop("'factorR' must be one finite non-zero numeric value.", call. = FALSE)
  }
  z <- extract_ts(x = pathRaster, polygons = shapefile, fun = "median", scale_factor = factorR)
  data.frame(ID = z$ID, Date = z$Date, Mean = z$Value, stringsAsFactors = FALSE)
}
