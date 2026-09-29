#' Legacy wrapper for missingness plotting
#' @param data Data frame returned by `GAM.missing()`.
#' @param doy_col,year_col,proporcion_col,prediccion_col Legacy column names.
#' @param sos,eos,max_doy Optional phenological markers.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' GAM.plot(proporciones)
#' GAM.plot(proporciones, sos = 11, eos = 163, max_doy = 74)
#' }
#' @export
GAM.plot <- function(data, doy_col = "DOY", year_col = "Year", proporcion_col = "Proporcion_Missing",
                     prediccion_col = "Predicted", sos = NULL, eos = NULL, max_doy = NULL) {
  .Deprecated("plot_missingness")
  d <- data.frame(Year = data[[year_col]], DOY = data[[doy_col]],
                  MissingProportion = data[[proporcion_col]], Predicted = data[[prediccion_col]])
  plot_missingness(d, sos = sos, max_doy = max_doy, eos = eos)
}
