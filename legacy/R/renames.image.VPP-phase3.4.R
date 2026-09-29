#' renames.image.VPP
#'
#' Rename TIF files in a directory according to the year extracted from the name.
#'
#' This function searches for TIF files in a specified directory and
#' renames them using the year extracted from the filename.
#' The new file name shall be in the format 'yyyy.tif'.
#'
#' @name renames.image.VPP
#'
#'
#' @param input_folder Path to the directory containing TIF files.
#'
#' @return Does not return a value, but renames the TIF files in the specified directory.
#' @export
#'
#' @examples
#' # Rename TIF files in the specified folder
#' rename_files("path/to/folder")

renames.image.VPP <- function(input_folder){
  #List the TIF files in the folder
  archivos <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
  length(archivos)
  # Browse the files and rename them
  for (archivo in archivos) {
    # Extract the date inside the image name (format yyyy)
    date <- gsub(".*(\\d{4}).*", "\\1", basename(archivo))# to rename files with the date format 'yyyy'.
    # New file name with date
    new.name <- paste0(input_folder, "/ ", date, ".tif")
    # Rename file
    file.rename(archivo, new.name)
  }
}
