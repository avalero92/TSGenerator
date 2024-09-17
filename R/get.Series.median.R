#' get.Series.median
#'
#' Get Series from Raster Files
#'
#' This function extracts time series reflectance data from raster files in TIFF format
#' and calculates the median of the values within an area defined by a shapefile.
#'@name get.Series.median
#'
#' @param pathRaster Path to the directory containing the raster files in TIFF format.
#' @param shapefile An object of class 'sf' defining the area of interest for extraction.
#' @param factorR A number representing the scale factor for the reflectance bands.
#'
#' @return A data frame containing three columns: ID (shapefile identifier),
#'         Date (date corresponding to the raster file) and Median (median of the extracted values).
#' @export
#'
#' @examples
#' Example of use: #' #' result <- get_series(pathRaster = "path/to/them/tifs",
#'#' shapefile = my_shapefile, #' factorR = 10000)
get.Series.median <- function(pathRaster = NULL, shapefile = NULL, factorR = NULL) {
  options(warn = -1)

  if (is.null(factorR)) {
    message("Please provide a factor scale for reflectance bands")
    stop("Factor scale is required")
  } else {
    factor <- factorR
  }

  files <- list.files(pathRaster, pattern = "\\.tif$", full.names = TRUE)
  dates <- list.files(pathRaster, pattern = "\\.tif$", full.names = FALSE)
  dates <- extract_dates_from_tiff_files(dates)

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
