#' Legacy aggregate quality interface
#'
#' @param df A legacy quality table.
#' @param period_col,observations_col Legacy column names.
#' @return This function stops with migration guidance.
#' @export
general.Quality <- function(df, period_col, observations_col) {
  .Deprecated("assess_ts_quality")
  stop("`general.Quality()` has been retired because it mixed aggregation, arbitrary quality classes, and interactive plotting. Use `assess_ts_quality()` for temporal completeness and plot its returned data explicitly with `ggplot2` if needed.", call. = FALSE)
}
