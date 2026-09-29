#' Legacy VPP file renaming helper
#'
#' TSGenerator 2.0 parses standard CLMS VPP filenames directly and no longer
#' renames source rasters, preserving product provenance and season metadata.
#' @param ... Legacy arguments.
#' @export
renames.image.VPP <- function(...) {
  stop("renames.image.VPP() is retired. TSGenerator 2.0 parses standard VPP filenames directly; use extract_vpp() without renaming the source files.", call. = FALSE)
}
