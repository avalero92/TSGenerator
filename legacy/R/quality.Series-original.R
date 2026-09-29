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
#' Estimate the quality of NDVI time series in different plots.
#' quality.Series(df = data, id_col = "ID", fecha_col = "Date", value_col = "NDVI")
quality.Series<- function(df, id_col, fecha_col, value_col) {
  # Asegurarse de que las columnas sean del tipo correcto
  df[[fecha_col]] <- as.Date(df[[fecha_col]])

  # Generate 91-day cut-offs
  start <- floor_date(min(df[[fecha_col]]), "day")
  end <- ceiling_date(max(df[[fecha_col]]), "day")
  cut <- seq(start, end, by = "91 days")

  # Initialize an empty dataframe for storing results
  df_calidad <- data.frame(id_col = character(),
                           period = character(),
                           observations = numeric(),
                           stringsAsFactors = FALSE)  # Specify not to convert to factors

  # Obtain the list of unique IDs
  ids <- unique(df[[id_col]])

  # Loop over each ID
  for (id in ids) {
    # Filter the dataframe for the current ID
    df_id <- df %>% filter(!!rlang::sym(id_col) == id)

    # Count observations in each period
    for (i in seq_len(length(cut) - 1)) {  # -1 to avoid the last index
      inicio_periodo <- cut[i]
      fin_periodo <- cut[i + 1]

      # Filter by period using filtered df_id
      observations <- df_id %>%
        filter(!!rlang::sym(fecha_col) >= inicio_periodo & !!rlang::sym(fecha_col) < fin_periodo) %>%
        summarise(observations = sum(!is.na(!!rlang::sym(value_col)), na.rm = TRUE)) %>%
        pull(observations)

      # If there are no observations, assign zero
      if (length(observations) == 0) {
        observations <- 0
      }

      # Add results to dataframe df_quality correctly
      df_calidad <- rbind(df_calidad,
                          data.frame(id_col = id,  # Using the correct assignment
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

  # Specify the id_col column again
  colnames(df_calidad)[which(colnames(df_calidad) == "id_col")] <- id_col

  # Create line chart with custom colors
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Diagonal labels
    facet_wrap(as.formula(paste("~", id_col)))

  # Convert to interactive graphic with plotly
  interactive_plot <- ggplotly(p)

  # Return graph and dataframe
  return(list(plot = interactive_plot, data = df_calidad))
}
