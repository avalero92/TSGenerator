library(shiny)
library(reticulate)
library(magick)  # Para manejo de imágenes
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para funcionalidad JavaScript mejorada
library(shinycssloaders)  # Para indicadores de carga

# Función para configurar Python
configurar_python <- function(ruta_python) {
  tryCatch({
    if (file.exists(ruta_python)) {
      use_python(ruta_python, required = TRUE)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    return(FALSE)
  })
}

# Función de validación de entradas
validate_inputs <- function(input) {
  errors <- character()

  if (input$user == "") errors <- c(errors, "Usuario es requerido")
  if (input$password == "") errors <- c(errors, "Contraseña es requerida")
  if (input$dataset_id == "") errors <- c(errors, "ID del Dataset es requerido")
  if (input$ruta_python == "") errors <- c(errors, "Ruta de Python es requerida")
  if (input$download_path == "") errors <- c(errors, "Ruta de descarga es requerida")

  # Validar formato de fechas
  if (input$start != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$start)) {
      errors <- c(errors, "Formato de fecha de inicio inválido (debe ser YYYY-MM-DD)")
    }
  }

  if (input$end != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$end)) {
      errors <- c(errors, "Formato de fecha de fin inválido (debe ser YYYY-MM-DD)")
    }
  }

  # Validar formato bbox si se proporciona
  if (input$bbox != "") {
    bbox_values <- tryCatch({
      as.numeric(unlist(strsplit(input$bbox, ",")))
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(bbox_values) || length(bbox_values) != 4) {
      errors <- c(errors, "Formato de Bbox inválido (debe ser: xmin,ymin,xmax,ymax)")
    }
  }

  return(errors)
}
