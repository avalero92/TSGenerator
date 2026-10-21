#' Retired legacy alias for phenology-specific missing counts
#'
#' Calling this wrapper emits a deprecation warning and then signals a
#' migration error through the retired `count_missing()` interface.
#' @inheritParams count_missing
#' @return No return value. This retired compatibility function signals an
#'   error with migration guidance; use `summarize_missingness()` instead.
#' @export
count.NA <- function(data, sos, maxd, eos, doy_col = "DOY", year_col = "Year", na_col = "NDVI", fid_col = "ID") {
  .Deprecated("summarize_missingness")
  count_missing(data, sos, maxd, eos, doy_col, year_col, na_col, fid_col)
}
