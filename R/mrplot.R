#' mrplot
#'
#' @import VIM
#' @import dplyr
#'
#' @name mrplot
#'
#' @param data Dataframe type object to store the time series
#' @param doy_col Column with Julian days
#' @param serie_col Column that stores the variable of interest
#' @param colors Selection of the colors of the graphic
#'
#' @return Graph showing the relationship between "NA" and "NO NA" observations with Julian days
#' @export
#'
#' @examples
#'
#'mrplot(Zaidin,doy_col = "DOY",serie_col = "NDVI_median", colors = c ("green4", "orange4"))
#'
mrplot <- function(data, doy_col = "DOY", serie_col = "NDVI", colors = c("green4", "orange")) {
  # Comprobar que las columnas existen
  if (!(doy_col %in% names(data)) || !(serie_col %in% names(data))) {
    stop("Specified columns do not exist in the dataframe.")
  }

  # Seleccionar las columnas necesarias
  selected_data <- data %>% select(!!sym(doy_col), !!sym(serie_col))

  # Crear el gráfico de marginplot
  marginplot(selected_data, col = colors)
}


