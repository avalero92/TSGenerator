#' GAM.plot
#'
#' @import scales
#'
#' @param data A DataFrame that contains the data to be plotted.
#' @param doy_col Name of the column representing the Julian Day (DOY).
#' @param year_col Name of the column representing the Year.
#' @param proporcion_col Name of the column representing the Proportion of Missing Data.
#' @param prediccion_col Name of the column representing the Prediction.
#' @param sos Optional value for the SOS line.
#' @param eos Optional value for the EOS line.
#' @param max_doy Optional value for the MAX line.
#'
#' @return
#' @export
#'
#' @examples
#' # Ejemplo de uso
#' # Without graphing phenological metrics
#'GAM.plot(proporciones)
#'
#' # With phenological metrics
#'GAM.plot(proporciones, sos = 11, eos = 163, max_doy = 74)

GAM.plot <- function(data,
                     doy_col = "DOY",
                     year_col = "Year",
                     proporcion_col = "Proporcion_Missing",
                     prediccion_col = "Predicted",
                     sos = NULL,
                     eos = NULL,
                     max_doy = NULL) {
  library(ggplot2)
  library(scales)

  # Verificar si el DataFrame es válido
  if (!is.data.frame(data)) {
    stop("Error: El argumento 'data' debe ser un DataFrame.")
  }

  # Verificar si las columnas existen
  columnas_requeridas <- c(doy_col, year_col, proporcion_col, prediccion_col)
  columnas_faltantes <- setdiff(columnas_requeridas, names(data))

  if (length(columnas_faltantes) > 0) {
    stop(paste("Error: The following columns are missing in the DataFrame:", paste(columnas_faltantes, collapse = ", ")))
  }

  # Crear el gráfico
  p <- ggplot(data, aes_string(x = doy_col, y = proporcion_col, color = paste("as.factor(", year_col, ")"))) +
    geom_point() +
    geom_line(aes_string(y = prediccion_col), size = 1) +  # Curva suavizada
    labs(
      title = "Missing Data Ratio with Smoothed GAM Curve",
      x = "Julian Day (DOY)",
      y = "Missing Data Ratio",
      color = "Year"
    ) +
    theme_bw() +
    facet_wrap(as.formula(paste("~", year_col))) +
    scale_x_continuous(breaks = seq(1, 365, by = 21)) +  # Ajusta el rango según tus datos
    scale_y_continuous(labels = percent_format(scale = 100))

  # Añadir líneas y anotaciones si están definidas
  if (!is.null(sos)) {
    p <- p + geom_vline(xintercept = sos, linetype = "dashed", color = "seagreen3") +
      annotate("text", x = sos, y = 0.75, label = "SOS", color = "seagreen3", vjust = -1)
  }

  if (!is.null(max_doy)) {
    p <- p + geom_vline(xintercept = max_doy, linetype = "dashed", color = "skyblue3") +
      annotate("text", x = max_doy, y = 0.75, label = "MAX", color = "skyblue3", vjust = -1)
  }

  if (!is.null(eos)) {
    p <- p + geom_vline(xintercept = eos, linetype = "dashed", color = "indianred3") +
      annotate("text", x = eos, y = 0.75, label = "EOS", color = "indianred3", vjust = -1)
  }

  # Mostrar el gráfico
  print(p)
}

