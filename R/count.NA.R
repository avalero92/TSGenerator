#' count.NA
#'
#' @import lubridate
#' @import dplyr
#' @import tidyverse
#'
#' @name count.NA
#'
#' @param data Dataframe where the variables DOY; Year; variable of interest and identifying variable that stores the identifiers of each polygon/plot are stored.
#' @param sos Opening day of the season
#' @param maxd Maximum growth day
#' @param eos End of season day
#' @param doy_col Column storing Julian days
#' @param year_col Column that stores the years in %yyyyy format
#' @param na_col Variable of interest
#' @param fid_col Column that stores the identifiers of each polygon/plot.
#'
#' @return A list type object that stores two dataframes: 1) Total NA by days and by polygons/plots; 2) Total NA by days and plots and the critical periods of the vegetation.
#' @export
#'
#' @examples
#' sos <- 47  # Replace with your real value
#' maxd <- 105  # Replace with your real value
#' eos <- 151  # Replace with your real value
#' Call the function with your data
#' result <- count.NA(datos, sos, maxd, eos,doy_col = "DOY", year_col = "Year", na_col = "NDVI_median", fid_col = "fid")
#' print(result$interactive_plot)

count.NA <- function(data, sos, maxd, eos, doy_col = "DOY", year_col = "Year", na_col = "NDVI", fid_col = "ID") {
  # Contar el número de parcelas (fid) con valor NA en la columna especificada por DOY y Year
  na_count_by_doy_year <- data %>%
    group_by(!!sym(doy_col), !!sym(year_col), !!sym(fid_col)) %>%
    summarize(has_na = any(is.na(!!sym(na_col))), .groups = 'drop') %>%
    group_by(!!sym(doy_col), !!sym(year_col)) %>%
    summarize(na_parcels = sum(has_na), .groups = 'drop')

  # Crear una matriz pivot para visualizar los resultados
  na_count_pivot <- na_count_by_doy_year %>%
    pivot_wider(names_from = !!sym(year_col), values_from = na_parcels)

  # Mostrar la matriz
  print(na_count_pivot)

  # Calcular el total de NA en los intervalos
  total_na_counts <- data %>%
    mutate(interval = case_when(
      !!sym(doy_col) < sos ~ "Before the SOS",
      !!sym(doy_col) >= sos & !!sym(doy_col) <= maxd ~ "Between SOS and MAXD",
      !!sym(doy_col) > maxd & !!sym(doy_col) <= eos ~ "Between MAXD and EOS",
      !!sym(doy_col) > eos ~ "After EOS",
      TRUE ~ "Out of range"
    )) %>%
    group_by(interval) %>%
    summarize(total_na = sum(is.na(!!sym(na_col))), .groups = 'drop')

  # Mostrar el total de NA por intervalo
  print(total_na_counts)

  # Crear el gráfico
  p <- ggplot(na_count_by_doy_year, aes(x = !!sym(doy_col), y = na_parcels, color = factor(!!sym(year_col)))) +
    geom_line() +
    geom_point() +
    labs(x = "DOY", y = "Total observations NA/day", color = "Year") +
    facet_wrap(as.formula(paste("~", year_col))) +  # Crea paneles separados para cada año
    scale_x_continuous(breaks = seq(1, max(data[[doy_col]], na.rm = TRUE), 20)) +
    theme_bw() +
    theme(legend.position = "")  # Ocultar la leyenda

  # Agregar líneas perpendiculares para SOS, MAXD y EOS
  p <- p +
    geom_vline(xintercept = sos, linetype = "dashed", color = "green3") +
    geom_vline(xintercept = maxd, linetype = "dashed", color = "orange3") +
    geom_vline(xintercept = eos, linetype = "dashed", color = "red3") +
    annotate("text", x = sos, y = max(na_count_by_doy_year$na_parcels, na.rm = TRUE),
             label = "SOS", vjust = -0.5, color = "black") +
    annotate("text", x = maxd, y = max(na_count_by_doy_year$na_parcels, na.rm = TRUE),
             label = "MAXD", vjust = -0.5, color = "black") +
    annotate("text", x = eos, y = max(na_count_by_doy_year$na_parcels, na.rm = TRUE),
             label = "EOS", vjust = -0.5, color = "black")

  # Convertir a gráfico interactivo
  interactive_plot <- ggplotly(p)

  # Devolver el gráfico y los data frames
  return(list(interactive_plot = interactive_plot, na_count_pivot = na_count_pivot, total_na_counts = total_na_counts))
}

