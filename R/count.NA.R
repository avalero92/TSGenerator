#' Legacy alias for phenology-specific missing counts
#' @inheritParams count_missing
#' @return This function stops with migration guidance.
#' @export
count.NA <- function(data, sos, maxd, eos, doy_col = "DOY", year_col = "Year", na_col = "NDVI", fid_col = "ID") {
  .Deprecated("summarize_missingness")
  count_missing(data, sos, maxd, eos, doy_col, year_col, na_col, fid_col)
}
