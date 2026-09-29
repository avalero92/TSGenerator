library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

# Definición de la función renames.image.IV
renames.image.IV <- function(input_folder = NULL) {
  # Listar los archivos TIF en la carpeta
  archivos <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Verificar si hay archivos para renombrar
  if (length(archivos) == 0) {
    stop("No se encontraron archivos TIF en la carpeta especificada.")
  }
  
  # Recorrer los archivos y renombrarlos
  for (archivo in archivos) {
    # Extraer la fecha dentro del nombre de la imagen (formato yyyy)
    date <- gsub(".*(\\d{8}).*", "\\1", basename(archivo)) # Para renombrar archivos con el formato de fecha 'yyyymmdd'.
    
    # Nuevo nombre del archivo con la fecha
    new.name <- file.path(input_folder, paste0(date, ".tif"))
    
    # Renombrar el archivo
    file.rename(archivo, new.name)
  }
}
