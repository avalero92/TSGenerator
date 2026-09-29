#' Legacy Kalman imputation wrapper
#'
#' `TsImpute()` is retained for TSGenerator 1.x compatibility. New workflows
#' should use [impute_ts()], which provides explicit temporal safeguards and
#' preserves provenance of imputed values.
#'
#' @param data A data frame.
#' @param group_col Group identifier column.
#' @param value_col Numeric value column.
#' @return The input data with a `<value_col>_completed` column.
#' @examples
#' \dontrun{
#' new_Data <- TsImpute(df, group_col = "ID", value_col = "NDVI")
#' }
#' @export
TsImpute <- function(data, group_col, value_col) {
  .Deprecated("impute_ts")
  out <- impute_ts(
    data = data,
    id_col = group_col,
    date_col = if ("Date" %in% names(data)) "Date" else stop("Legacy `TsImpute()` now requires a `Date` column. Use `impute_ts()` for explicit column selection.", call. = FALSE),
    value_col = value_col,
    method = "kalman",
    series_type = "user",
    allow_edge = TRUE,
    output_col = paste0(value_col, "_completed")
  )
  out$.WasImputed <- NULL
  out$.InsertedDate <- NULL
  class(out) <- "data.frame"
  out
}
