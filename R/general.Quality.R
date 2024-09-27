#' general.Quality
#'
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @import plotly
#'
#'
#' @name general.Quality
#'
#' @param df Data base generated from the use of the function quality.Series
#' @param period_col Column where the periods are stored (every 91 days)
#' @param observations_col Columna donde se almacenan el numero de observaciones por periodo y
#'
#' @return A list of files that stores a new data frame and an interactive graphic.
#' @export
#'
#' @examples
#' Estimation of the average quality of all NDVI time series.
#' general.Quality(df = "Data base", period_col = "Periods", observations_col = "Observations")
general.Quality <- function(df, period_col, observations_col) {
  # We group and calculate the average
  nuevo_df <- df %>%
    group_by(!!sym(period_col)) %>%
    summarize(mean = mean(!!sym(observations_col), na.rm = TRUE)) %>%
    mutate(average_quality = mean,
           Quality = case_when(
             mean < 1 ~ "Very bad",
             mean >= 1 & mean < 3 ~ "Low",
             mean >= 3 & mean <= 8 ~ "Medium",
             mean > 8 ~ "High",
             TRUE ~ "Unknown"
           ))  # We add the quality column

  # We create the interactive bar chart for the mean values.
  Mean <- ggplot(nuevo_df, aes_string(x = period_col, y = "mean", fill = "Quality")) +
    geom_point()+
    geom_bar(stat = "identity") +
    labs(title = "Average number of observations per period",
         x = period_col,
         y = "Average of Observations") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Diagonal labels
    scale_fill_manual(values = c("Very bad" = "red",
                                 "Low" = "orange",
                                 "Medium" = "yellow",
                                 "High" = "green",
                                 "Unknown" = "grey"))  # Custom colors

  # We convert to interactive graphics with plotly
  grafico_interactivo <- ggplotly(Mean)

  # We return the interactive graphic and the new data frame
  return(list(grafico = grafico_interactivo, data = nuevo_df))
}



