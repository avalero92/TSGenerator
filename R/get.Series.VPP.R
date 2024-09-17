#' get.Series.VPP
#'
#' Extract and process data from HR-VPP derived TIFF raster files using a shapefile.
#'
#' This function allows the extraction of band reflectance values from raster images in TIFF format
#' using a shapefile to define the areas of interest. The extracted values are processed and
#' returned as a data frame.
#'
#' @name get.Series.VPP
#'
#' @param pathRaster Path to the directory containing raster files in TIFF format.
#' @param shapefile sf' object defining the areas of interest for data mining.
#' @param factorR Factor scale for reflectance bands. It is a numerical value that is used
#'        to adjust the extracted values.
#'
#' @return A data frame containing the ID of the areas of interest, the date corresponding to each
#'         TIFF file and the adjusted mean reflectance values.
#' @export
#'
#' @examples
#' Extracting indexes from a raster directory and a shapefile
#' índices <- extraer_índices("ruta/a/carpeta", shapefile = mi_shapefile, factorR = 10000)
function(pathRaster = NULL, shapefile = NULL, factorR = NULL) {
  options(warn = -1)

  if (is.null(factorR)) {
    message("Please provide a factor scale for reflectance bands")
    stop("Factor scale is required")
  } else {
    factor <- factorR
  }

  files <- list.files(pathRaster, pattern = "\\.tif$", full.names = TRUE)
  dates <- list.files(pathRaster, pattern = "\\.tif$", full.names = FALSE)
  #dates <- extract_dates_from_tiff_files(dates)

  if (length(files) == 0) {
    message("Please provide raster files in TIFF format")
    stop("TIF format is needed")
  } else {
    message("Processing rasters ...")
    progress_bar <- txtProgressBar(min = 0, max = length(files), style = 3, char = "=")
    list.indices <- list()

    for (k in seq_along(files)) {
      setTxtProgressBar(progress_bar, k)
      rs <- brick(files[k])

      if (class(shapefile)[1] == "sf") {
        r.extract<-crop( rs,extent(shape))
        r.extract_1 <- raster::extract(r.extract, shapefile, fun = median, na.rm = TRUE)
        data.write <- cbind(ID = rownames(shape), Date = rep(dates[k], length(r.extract_1)), Mean = r.extract_1/factorR)
        list.indices[[k]] <- data.write
      } else {
        message("Please provide an 'sf' object for extraction.")
        stop("An 'sf' object is needed")
      }
    }
  }

  close(progress_bar)
  data.export <- do.call(rbind.data.frame, list.indices)
  return(data.export)
}
