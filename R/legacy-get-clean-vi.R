#' Legacy VI cleaning workflow
#' @param ... Legacy arguments.
#' @export
get.Clean.IV <- function(...) {
  stop("get.Clean.IV() belongs to the discontinued VI/QFLAG2 workflow and is no longer executed by the TSGenerator 2.0 core. The historical implementation is archived under legacy/R/. For current ST quality handling use mask_quality().", call. = FALSE)
}
