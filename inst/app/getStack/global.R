library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

# Supongamos que la función get.Stack está definida en otro lugar
# Aquí solo se incluye como un placeholder para la función real
get.Stack <- function(IV_path, QFLAG, output_path) {
  # Simulación de la función para propósitos de demostración
  Sys.sleep(2)  # Simula tiempo de procesamiento
  return(paste("Stacks raster creados en:", output_path))
}