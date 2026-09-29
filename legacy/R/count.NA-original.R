#' Count missing observations (legacy name)
#'
#' Compatibility wrapper for `count_missing()`.
#'
#' @inheritParams count_missing
#' @return The same value as `count_missing()`.
#' @export
count.NA <- function(data, sos, maxd, eos, doy_col = "DOY", year_col = "Year", na_col = "NDVI", fid_col = "ID") {
  warning("`count.NA()` is retained for TSGenerator 1.x compatibility; use `count_missing()` in new code.", call. = FALSE)
  count_missing(data, sos, maxd, eos, doy_col, year_col, na_col, fid_col)
}
