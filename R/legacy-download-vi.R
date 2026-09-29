#' Legacy VI downloader (discontinued)
#'
#' The pan-European HR-VPP vegetation-index products targeted by this function
#' are no longer disseminated. The function is retained only to provide a clear
#' migration error and deliberately contains no Python/reticulate dependency.
#' @param ... Legacy arguments, ignored.
#' @export
Download.VI <- function(...) {
  stop("'Download.VI()' is no longer operational because the targeted HR-VPP VI products were discontinued. Use current ST/VPP acquisition via download_st() or download_vpp().", call. = FALSE)
}
