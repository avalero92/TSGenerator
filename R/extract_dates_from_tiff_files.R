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
