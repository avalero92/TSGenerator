#' Legacy aggregate quality interface
#'
#' `general.Quality()` is retained only as a TSGenerator 1.x migration guard.
#' The legacy workflow combined aggregation, arbitrary quality classes, and
#' interactive plotting and is therefore not reproduced in the 2.0 analytical
#' core. New workflows should use [assess_ts_quality()] for temporal
#' completeness assessment and plot the returned data explicitly with
#' `ggplot2` when visualization is required.
#'
#' @param df A legacy quality table.
#' @param period_col,observations_col Legacy column names.
#' @return This function stops with migration guidance.
#' @export
general.Quality <- function(df, period_col, observations_col) {
  .Deprecated("assess_ts_quality")
  stop("`general.Quality()` has been retired because it mixed aggregation, arbitrary quality classes, and interactive plotting. Use `assess_ts_quality()` for temporal completeness and plot its returned data explicitly with `ggplot2` if needed.", call. = FALSE)
}
