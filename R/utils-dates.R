# Internal date parser used by raster time-series extractors.
# Accepts YYYY-MM-DD or YYYYMMDD anywhere in the basename.
.extract_dates_from_tiff_files <- function(tiff_files) {
  if (!is.character(tiff_files) || length(tiff_files) == 0L) {
    stop("'tiff_files' must be a non-empty character vector.", call. = FALSE)
  }
  x <- basename(tiff_files)
  dates <- stringr::str_extract(x, "\\d{4}-\\d{2}-\\d{2}|\\d{8}")
  missing <- is.na(dates)
  if (any(missing)) {
    stop(
      paste0("Could not extract a date from: ", paste(x[missing], collapse = ", "),
             ". Expected YYYY-MM-DD or YYYYMMDD in each filename."),
      call. = FALSE
    )
  }
  dates
}
