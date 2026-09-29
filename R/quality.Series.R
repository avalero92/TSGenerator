#' Legacy time-series quality interface
#'
#' `quality.Series()` belongs to the TSGenerator 1.x 91-day-window workflow.
#' New analyses should use [summarize_missingness()] and [assess_ts_quality()],
#' which distinguish absent expected dates from explicit missing values.
#'
#' @param df A data frame.
#' @param id_col,fecha_col,value_col Legacy column names.
#' @return This function stops with migration guidance.
#' @export
quality.Series <- function(df, id_col, fecha_col, value_col) {
  .Deprecated("assess_ts_quality")
  stop("`quality.Series()` has been retired from the active 2.0 analytical core. Use `assess_ts_quality()` (and `summarize_missingness()` when counts are required). The original implementation is archived under `legacy/R/`.", call. = FALSE)
}
