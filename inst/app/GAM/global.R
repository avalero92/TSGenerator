# global.R --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(dplyr)
library(mgcv)
library(readr)

# Función GAM.missing() - definida globalmente para que esté disponible en server.R
GAM.missing <- function(data, year_col, doy_col, missing_col) {
  library(dplyr)
  library(mgcv)
  if (missing(year_col) || missing(doy_col) || missing(missing_col)) {
    stop("Debe proporcionar los nombres de columna para year_col, doy_col y missing_col.")
  }
  if (!all(c(year_col, doy_col, missing_col) %in% names(data))) {
    stop("Una o más columnas especificadas no existen en los datos.")
  }
  proporciones <- data %>%
    group_by(!!sym(year_col), !!sym(doy_col)) %>%
    summarise(
      Total = n(),
      Missing = sum(is.na(!!sym(missing_col))),
      Proporcion_Missing = Missing / Total,
      .groups = "drop"
    )
  formula_gam <- as.formula(paste("cbind(Missing, Total - Missing) ~ s(", doy_col, ", bs = 'cs') + ", year_col))
  gam_model <- gam(formula_gam, family = binomial(link = "logit"), data = proporciones)
  proporciones$Predicted <- predict(gam_model, type = "response")
  return(list(proporciones = proporciones, model = gam_model))
}

# Función auxiliar para adivinar separador (opcional, pero útil)
guess_delim <- function(file) {
  first_line <- readLines(file, n = 1)
  counts <- c(
    comma = length(gregexpr(",", first_line)[[1]]),
    semicolon = length(gregexpr(";", first_line)[[1]]),
    tab = length(gregexpr("\t", first_line)[[1]])
  )
  names(which.max(counts))
}
