#' get.Series.mean
#'
#'Extracts and processes reflectance indices from TIFF raster files using a shapefile.
#'
#' This function allows the extraction of mean band reflectance values from raster images in TIFF format.
#' using a shapefile to define the areas of interest. The extracted values are processed and
#' returned as a data frame.
#'
#' @name get.Series.mean
#'
#' @param pathRaster Path to the directory containing raster files in TIFF format.
#' @param shapefile sf' object defining the areas of interest for data mining.
#' @param factorR Factor scale for reflectance bands. It is a numerical value that is used
#'        to adjust the extracted values.
#'
#' @return A data frame containing the ID of the areas of interest, the date corresponding to each
#' #' TIFF file and the adjusted mean reflectance values.
#' @export
#'
#' @examples
#' # Extracting indexes from a raster directory and a shapefile
#' índices <- extraer_índices("ruta/a/carpeta", shapefile = mi_shapefile, factorR = 10000)
get.Series.mean <- function(pathRaster = NULL, shapefile = NULL, factorR = NULL) {
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
        r.extract_1 <- raster::extract(r.extract, shapefile, fun = mean, na.rm = TRUE)
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
