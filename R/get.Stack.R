#' get.Stack
#'
#' rocesses vegetation index files and adds QFLAG layers.
#'
#' Ehis function searches for .TIFF files of vegetation indexes (VI) in a specified directory,
#' and for each file, it tries to find a corresponding QFLAG file.
#' If found, the QFLAG file is re-scaled to match
#' the resolution of the VI file and is added as a new layer.
#' Finally, the resulting file is saved in an output directory.
#'
#' @name get.Stack
#'
#' @param IV_path Path to the directory containing VI files in .tif format.
#' @param QFLAG Path to the directory containing QFLAG files.
#' @param output_path Path to the directory where the processed files will be saved.
#'
#' @return It does not return a value, but saves .tif files in the output directory
#'         with the NDVI and QFLAG layers combined.
#' @export
#'
#' @examples
#' # Processing VI files and adding QFLAG layers
#' process_files("path/to/VI", "path/to/QFLAG", "path/to/output")
function(IV_path, QFLAG, output_path) {
  archivos <- list.files(IV_path, pattern = "\\.tif$", full.names = TRUE)
  clusters <- detectCores() - 1
  cl <- makePSOCKcluster(clusters)
  set.seed(1234)

  for (archivo in archivos) {
    # Get date from NDVI file
    fecha <- gsub(".*_(\\d{8})_.*", "\\1", basename(archivo))
    # Path of the QFLAG file corresponding to the date
    archivo_qflag <- file.path(QFLAG, paste0(fecha))

    # Check if the QFLAG file exists
    if (file.exists(archivo_qflag)) {

      # Read NDVI and QFLAG files
      raster <- brick(archivo)
      qflag <- brick(archivo_qflag)

      # Resampling the QFLAG file to the resolution of the NDVI file
      qflag_resampled <- raster::resample(qflag, raster, method = "bilinear")

      # Verify that dimensions match
      if (nrow(raster) == nrow(qflag_resampled) && ncol(raster) == ncol(qflag_resampled)) {
        # Add resampled QFLAG2 band to NDVI file
        raster_sumado <- addLayer(raster, qflag_resampled)
        # Save updated file with both bands
        writeRaster(raster_sumado, file.path(output_path, paste0(fecha)), format = "GTiff", overwrite = TRUE)
      } else {
        print(paste("Image dimensions and QFLAG for the date", fecha, "do not match. The sum could not be added up."))
      }
    } else {
      print(paste("QFLAG file not found for date", fecha, "in the QFLAG folder. The addition could not be performed."))
    }
  }

  stopCluster(cl)
}
