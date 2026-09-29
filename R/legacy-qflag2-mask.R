#' Legacy VI QFLAG2 masking workflow
#' @param ... Legacy arguments.
#' @export
QFLAG2.Mask <- function(...) {
  stop("QFLAG2.Mask() targets the discontinued VI QFLAG2 product and is retired from the active 2.0 core. The historical implementation is archived under legacy/R/. Use quality_info(), summarize_quality(), classify_quality() and mask_quality() for current ST/VPP quality products.", call. = FALSE)
}
