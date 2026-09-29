#'renames.image.IV
#'
#'Rename TIF files in a directory according to the date contained in the name.
#'
#' This function looks for TIF files in a specified directory and
#' rename them using the date extracted from the file name.
#' The new file name will have the format 'yyyymmdd.tif'.
#'
#'@name
#'renames.image.IV
#'
#'
#'
#' @param input_folder Path to the directory containing TIF files.
#'
#' @return It does not return a value, but renames the TIF files in the specified directory.
#' @section Legacy status:
#' This function is retained for TSGenerator 1.x compatibility and is not part of the planned TSGenerator 2.x core API.
#' @export
#'
#' @examples
#' # Rename TIF files in the specified folder
#' rename_files("path/to/folder")
renames.image.IV <- function(input_folder=NULL){
  warning("renames.image.IV() targets discontinued VI products and is retained only for TSGenerator 1.x compatibility.", call. = FALSE)
  #Listar los archivos TIF enla carpeta
  archivos <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
  length(archivos)
  # Recorrer los archivos y renombrarlos
  for (archivo in archivos) {
    # Extraer la fecha dentro del nombre de la imagen (formato yyyy)
    date <- gsub(".*(\\d{8}).*", "\\1", basename(archivo))# to rename files with the date format 'yyyymmdd'.
    # Nuevo nombre del archivo con la fecha
    new.name <- paste0(input_folder, "/ ", date, ".tif")
    # Renombrar el archivo
    file.rename(archivo, new.name)
  }
}
