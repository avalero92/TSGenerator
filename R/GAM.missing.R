#' Legacy wrapper for missingness GAM
#' @param data Data frame containing the missingness time-series information.
#' @param year_col Name of the column containing years.
#' @param doy_col Name of the column containing day-of-year values.
#' @param missing_col Name of the column containing the missingness/value indicator used by the legacy wrapper.
#' @return Legacy-compatible list with `proporciones` and `model`.
#' @examples
#' \dontrun{
#' resultado <- GAM.missing(
#'   data,
#'   year_col = "Year",
#'   doy_col = "DOY",
#'   missing_col = "NDVI_median"
#' )
#' }
#' @export
GAM.missing <- function(data, year_col, doy_col, missing_col) {
  .Deprecated("model_missingness")
  z <- model_missingness(data, year_col = year_col, doy_col = doy_col, value_col = missing_col)
  p <- z$data
  names(p)[names(p) == "MissingProportion"] <- "Proporcion_Missing"
  list(proporciones = p, model = z$model)
}
