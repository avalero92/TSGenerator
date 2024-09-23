#' get.Series.median
#'
#' Get Series from Raster Files
#' @import utils
#' @import parallel
#' @import doParallel
#' @import raster
#' @import sf
#' @importFrom stats median
#'
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
        r.extract<-crop( rs,extent(shapefile))
        r.extract_1 <- raster::extract(r.extract, shapefile, fun = median, na.rm = TRUE)
        data.write <- cbind(ID = rownames(shapefile), Date = rep(dates[k], length(r.extract_1)), Mean = r.extract_1/factorR)
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


#' extract_dates_from_tiff_files
#'
#' @name extract_dates_from_tiff_files
#'
#' @param tiff_files List of files in .tif format
#'
#' @return The names of each of the files in the formats %Y-%m-%d or YYYYYYYMMDD
#' @export
#'
#' @examples
#' Esta función se utiliza dentro de get.Series.
extract_dates_from_tiff_files <- function (tiff_files)
{
  dates <- vector("list", length(tiff_files))
  for (i in seq_along(tiff_files)) {
    regex <- "\\d{4}-\\d{2}-\\d{2}|\\d{8}"
    date_str <- stringr::str_extract(tiff_files[i], regex)
    if (!is.na(date_str)) {
      dates[[i]] <- date_str
    }
    else {
      dates[[i]] <- "missing"
      stop("files should contained date in this format: %Y-%m-%d or YYYYMMDD")
    }
  }
  return(unlist(dates[!is.na(dates)]))
}

