# ============================================================================
# Librerías requeridas
# ============================================================================
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyFiles)
library(reticulate)
library(magick)
library(fs)
library(rlang)

# ============================================================================
# Operador auxiliar %||% (usado en el server para valores NULL)
# ============================================================================
# Este operador devuelve el primer valor si no es NULL, de lo contrario el segundo.
# Ejemplo: item$id %||% "N/A"
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
