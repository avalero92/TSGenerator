#' Extract dates from TIFF filenames (compatibility wrapper)
#'
#' This public helper is retained for compatibility with TSGenerator 1.x.
#' New package code uses an internal parser.
#'
#' @param tiff_files Character vector of TIFF file paths or names.
#' @return Character vector containing dates encoded as YYYY-MM-DD or YYYYMMDD.
#' @export
extract_dates_from_tiff_files <- function(tiff_files) {
  .extract_dates_from_tiff_files(tiff_files)
}
