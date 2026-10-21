#' Legacy wrapper for missingness plotting
#' @param data Data frame returned by `GAM.missing()`.
#' @param doy_col,year_col,proporcion_col,prediccion_col Legacy column names.
#' @param sos,eos,max_doy Optional phenological markers.
#' @return A ggplot object.
#' @examples
#' proportions <- data.frame(
#'   Year = rep(2020L, 5), DOY = c(20, 50, 80, 110, 140),
#'   Proporcion_Missing = c(0.1, 0.2, 0.3, 0.2, 0.1),
#'   Predicted = c(0.12, 0.18, 0.25, 0.18, 0.12)
#' )
#' p <- suppressWarnings(GAM.plot(proportions))
#' inherits(p, "ggplot")
#' @export
GAM.plot <- function(data, doy_col = "DOY", year_col = "Year", proporcion_col = "Proporcion_Missing",
                     prediccion_col = "Predicted", sos = NULL, eos = NULL, max_doy = NULL) {
  .Deprecated("plot_missingness")
  d <- data.frame(Year = data[[year_col]], DOY = data[[doy_col]],
                  MissingProportion = data[[proporcion_col]], Predicted = data[[prediccion_col]])
  plot_missingness(d, sos = sos, max_doy = max_doy, eos = eos)
}
