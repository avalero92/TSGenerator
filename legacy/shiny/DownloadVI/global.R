# Carga de librerías requeridas
library(shiny) # Framework principal para aplicaciones web
library(reticulate) # Interfaz R-Python
library(magick) # Procesamiento de imágenes
library(shinydashboard) # Componentes de dashboard
library(shinyjs) # Funcionalidades JavaScript avanzadas
library(shinycssloaders)# Indicadores de carga animados

# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

#' Configurar el entorno Python
#'
#' Esta función configura el intérprete de Python especificado y valida
#' que esté disponible para su uso con reticulate.
#'
#' @param ruta_python String con la ruta al ejecutable de Python
#' @return Boolean indicando éxito (TRUE) o fallo (FALSE)
#' @examples
#' configurar_python("/usr/bin/python3")
#' configurar_python("C:/Python38/python.exe")
configurar_python <- function(ruta_python) {
  tryCatch({
    if (file.exists(ruta_python)) {
      use_python(ruta_python, required = TRUE)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    message("Error configurando Python: ", e$message)
    return(FALSE)
  })
}

#' Validar entradas del usuario
#'
#' Realiza validación comprehensiva de todos los campos de entrada,
#' incluyendo formato de fechas, bbox y campos obligatorios.
#'
#' @param input Lista con los valores de entrada de Shiny
#' @return Vector de caracteres con mensajes de error (vacío si no hay errores)
validate_inputs <- function(input) {
  errors <- character()
  # Validación de campos obligatorios
  if (input$user == "") errors <- c(errors, "📧 Usuario es requerido")
  if (input$password == "") errors <- c(errors, "🔐 Contraseña es requerida")
  if (input$dataset_id == "") errors <- c(errors, "📊 ID del Dataset es requerido")
  if (input$download_path == "") errors <- c(errors, "📁 Ruta de descarga es requerida")
  if (input$ruta_python == "") errors <- c(errors, "🐍 Ruta de Python es requerida")
  # Validación de formato de fechas (YYYY-MM-DD)
  if (input$start != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$start)) {
      errors <- c(errors, "📅 Formato de fecha de inicio inválido (debe ser YYYY-MM-DD)")
    }
  }
  if (input$end != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$end)) {
      errors <- c(errors, "📅 Formato de fecha de fin inválido (debe ser YYYY-MM-DD)")
    }
  }
  # Validación de bbox (bounding box)
  if (input$bbox != "") {
    bbox_values <- tryCatch({
      as.numeric(unlist(strsplit(input$bbox, ",")))
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(bbox_values) || length(bbox_values) != 4) {
      errors <- c(errors, "🗺️ Formato de Bbox inválido (debe ser: xmin,ymin,xmax,ymax)")
    } else {
      # Validación adicional de rangos de coordenadas
      if (any(abs(bbox_values[c(1,3)]) > 180) || any(abs(bbox_values[c(2,4)]) > 90)) {
        errors <- c(errors, "🌍 Coordenadas fuera de rango válido (lon: ±180, lat: ±90)")
      }
    }
  }
  return(errors)
}
