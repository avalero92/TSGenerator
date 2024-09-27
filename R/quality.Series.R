#' quality.Series
#'
#' @import ggplot2
#' @import rlang
#' @import plotly
#' @import lubridate
#' @import dplyr
#'
#' @name quality.Series
#'
#' @param df Data base
#' @param id_col Column where the observation grid identifiers are located.
#' @param fecha_col Column where the date is located (%yyyyy%mm%dd)
#' @param value_col Column from which the quality of the time series will be determined (estimating the number of observations).
#'
#' @return A list of files where a dataframe with the results of the quality assessment and an interactive graphic are stored.
#' @export
#'
#' @examples
#' Estimar la calidad  series temporales de NDVI en diferentes parcelas
#' quality.Series(df = data, id_col = "ID", fecha_col = "Date", value_col = "NDVI")
quality.Series<- function(df, id_col, fecha_col, value_col) {
  # Asegurarse de que las columnas sean del tipo correcto
  df[[fecha_col]] <- as.Date(df[[fecha_col]])

  # Generar los cortes de 91 días
  start <- floor_date(min(df[[fecha_col]]), "day")
  end <- ceiling_date(max(df[[fecha_col]]), "day")
  cut <- seq(start, end, by = "91 days")

  # Inicializar un dataframe vacío para almacenar los resultados
  df_calidad <- data.frame(id_col = character(),
                           period = character(),
                           observations = numeric(),
                           stringsAsFactors = FALSE)  # Especificar que no convierta a factores

  # Obtener la lista de IDs únicos
  ids <- unique(df[[id_col]])

  # Bucle sobre cada ID
  for (id in ids) {
    # Filtrar el dataframe para el ID actual
    df_id <- df %>% filter(!!rlang::sym(id_col) == id)

    # Contar observaciones en cada periodo
    for (i in seq_len(length(cut) - 1)) {  # -1 para evitar el último índice
      inicio_periodo <- cut[i]
      fin_periodo <- cut[i + 1]

      # Filtrar por periodo usando el df_id filtrado
      observations <- df_id %>%
        filter(!!rlang::sym(fecha_col) >= inicio_periodo & !!rlang::sym(fecha_col) < fin_periodo) %>%
        summarise(observations = sum(!is.na(!!rlang::sym(value_col)), na.rm = TRUE)) %>%
        pull(observations)

      # Si no hay observaciones, asignar cero
      if (length(observations) == 0) {
        observations <- 0
      }

      # Añadir resultados al dataframe df_calidad correctamente
      df_calidad <- rbind(df_calidad,
                          data.frame(id_col = id,  # Usando la asignación correcta
                                     period = paste(inicio_periodo, "to", fin_periodo),
                                     observations = observations,
                                     stringsAsFactors = FALSE))
    }
  }

  # Classify quality
  df_calidad <- df_calidad %>%
    mutate(quality = case_when(
      observations < 1 ~ "Very bad",
      observations >= 1 & observations < 3 ~ "Low",
      observations >= 3 & observations <= 8 ~ "Medium",
      observations > 8 ~ "High",
      TRUE ~ "Unknown"
    ))

  # Especificar de nuevo la columna id_col
  colnames(df_calidad)[which(colnames(df_calidad) == "id_col")] <- id_col

  # Crear la gráfica de líneas con colores personalizados
  # Crear la gráfica de líneas con colores personalizados
  p <- ggplot(df_calidad, aes_string(x = "period", y = "observations", fill = "quality", group = id_col)) +
    geom_point()+
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Very bad" = "red",
                                 "Low" = "orange",
                                 "Medium" = "yellow",
                                 "High" = "green")) +
    labs(title = "Quality of Time Series",
         x = "Period (every 91 days)",
         y = "Number of Observations",
         fill = "Quality") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Etiquetas en diagonal
    facet_wrap(as.formula(paste("~", id_col)))  # Asegúrate de que id_col esté bien asignado

  # Convertir a gráfico interactivo con plotly
  interactive_plot <- ggplotly(p)

  # Devolver el gráfico y el dataframe
  return(list(plot = interactive_plot, data = df_calidad))
}
