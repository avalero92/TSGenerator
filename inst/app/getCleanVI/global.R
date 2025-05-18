library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga
library(raster)  # Para trabajar con archivos raster

# Definición de la función get.Clean.IV
get.Clean.IV <- function(stack_folder = NULL, output_folder) {
  if (is.null(stack_folder)) {
    message("Please insert an address to rastestack")
    stop("Address to rastestack is required")
  }
  
  # Obtener la lista de archivos raster en la carpeta
  raster_files <- list.files(path = stack_folder, pattern = ".tif$", full.names = TRUE)
  
  # Crear el directorio de salida si no existe
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Iterar sobre cada archivo raster
  for (file in raster_files) {
    # Abrir el raster multibanda
    r <- raster::stack(file)
    
    # Obtener la matriz de la banda "QFLAG2"
    banda2 <- r[[2]]
    
    # Aplicar condición para eliminar píxeles en la primera banda
    r[[1]][raster::values(banda2) != 1] <- NA
    
    # Obtener el nombre del archivo sin la ruta
    file_name <- basename(file)
    
    # Crear la ruta de salida para el archivo modificado
    output_file <- file.path(output_folder, file_name)
    
    # Guardar el archivo modificado en la carpeta de salida
    raster::writeRaster(r[[1]], filename = output_file, format = "GTiff", overwrite = TRUE)
  }
}
