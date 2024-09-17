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
#' @export
#'
#' @examples
#' # Rename TIF files in the specified folder
#' rename_files("path/to/folder")
function(input_folder=NULL){
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
