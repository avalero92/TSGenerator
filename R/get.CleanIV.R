#' get.Clean.IV
#'
#'
#' Obtain and clean raster images
#'
#' This function processes a set of raster files in a specified directory,,
#' removing pixels in the first band according to the values in the second band.
#'
#' @import raster
#'
#' @import sf
#'
#' @import doParallel
#'
#' @name get.Clean.IV
#'
#' @param stack_folder Path to the directory containing the raster files (.tif).
#' If NULL, an error message will be displayed.
#' @param output_folder ath to the directory where the processed raster files will be saved.
#'
#' @return It does not return any value, but saves the processed raster files in the output directory.
#' @export
#'
#' @examples
#' Calling the function with one input and one output directory
#'get.Clean.IV(stack_folder = "ruta/a/tu/directorio", output_folder = "ruta/a/directorio/salida")

function(stack_folder=NULL, output_folder) {

  if (is.null(stack_folder)) {
    message("Please insert an address to rastestack")
    stop("Address to rastestack is required")
  }

  # Get the list of raster files in the folder
  raster_files <- list.files(path = stack_folder, pattern = ".tif$", full.names = TRUE)

  # Create output directory if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Iterate on each raster file
  for (file in raster_files) {
    # Open multiband raste
    r <- raster::stack(file)

    # Obtain the band "QFLAG2" matrix
    banda2 <- r[[2]]

    # Apply condition to eliminate pixels in the first band
    r[[1]][raster::values(banda2) != 1] <- NA

    # Get file name without path
    file_name <- basename(file)

    # Create output path for the modified file
    output_file <- file.path(output_folder, file_name)

    # Save the modified file in the output folder
    raster::writeRaster(r[[1]], filename = output_file, format = "GTiff", overwrite = TRUE)
  }
}
