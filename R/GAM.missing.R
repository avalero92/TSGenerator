#' GAM.missing
#'
#' @param data Data frame containing the time series information of the vegetation index with presence of missing data
#' @param year_col Column storing the information of the years corresponding to each time series (yyyy)
#' @param doy_col Column that stores the information related to the Julian days
#' @param missing_col
#'
#' @return A list object where the prediction results and the GAM model data are stored.
#'
#' @examples
#' resultado <- GAM.missing(CB_M, "Year", "DOY", "NDVI_median")
#' print(resultado$proporciones)
#' summary(resultado$model)
#'
#' @export
#'

GAM.missing <- function(data, year_col, doy_col, missing_col) {

  # Cargar las librerías necesarias
  library(dplyr)
  library(mgcv)

  # Verificar si se han proporcionado las columnas necesarias
  if (missing(year_col) || missing(doy_col) || missing(missing_col)) {
    stop("Error: You must provide the column names 'year_col', 'doy_col' and 'missing_col'.")
  }

  # Verificar si las columnas existen en el DataFrame
  if (!all(c(year_col, doy_col, missing_col) %in% names(data))) {
    stop("Error: One or more specified columns do not exist in the DataFrame.")
  }

  # Calcular la proporción de datos faltantes agrupados por DOY y Year
  proporciones <- data %>%
    group_by(!!sym(year_col), !!sym(doy_col)) %>%
    summarise(
      Total = n(),
      Missing = sum(is.na(!!sym(missing_col))),  # Usa el nombre de la columna proporcionada
      Proporcion_Missing = Missing / Total
    ) %>%
    ungroup()

  # Ajustar el modelo GAM binomial
  formula_gam <- as.formula(paste("cbind(Missing, Total - Missing) ~ s(", doy_col, ", bs = 'cs') + ", year_col))

  gam_model <- gam(formula_gam, family = binomial(link = "logit"), data = proporciones)

  # Predecir valores suavizados
  proporciones$Predicted <- predict(gam_model, type = "response")

  # Devolver los resultados
  return(list(proporciones = proporciones, model = gam_model))
}

# Ejemplo de uso
resultado <- GAM.missing(CB_M, "Year", "DOY", "NDVI_median")
print(resultado$proporciones)
summary(resultado$model)
