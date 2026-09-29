#' Legacy phenology-specific missing-count interface
#'
#' `count_missing()` in TSGenerator 1.x counted missing parcel observations in
#' hard-coded SOS/MAXD/EOS intervals and returned a Plotly object. The 2.0 core
#' separates generic temporal completeness from phenological interpretation.
#'
#' @param data A data frame.
#' @param sos,maxd,eos Legacy day-of-year thresholds.
#' @param doy_col,year_col,na_col,fid_col Legacy column names.
#' @return This function stops with migration guidance.
#' @export
count_missing <- function(data, sos, maxd, eos, doy_col = "DOY", year_col = "Year", na_col = "NDVI", fid_col = "ID") {
  .Deprecated("summarize_missingness")
  stop("Legacy `count_missing()` has been retired from the active 2.0 core. Use `summarize_missingness()` for expected/absent/NA counts and `assess_ts_quality()` for completeness classes. Phenological interval summaries should be derived explicitly from those outputs.", call. = FALSE)
}
