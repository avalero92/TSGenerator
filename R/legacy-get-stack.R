#' Legacy VI/QFLAG stack workflow
#' @param ... Legacy arguments.
#' @export
get.Stack <- function(...) {
  stop("get.Stack() belongs to the discontinued VI/QFLAG2 workflow and is no longer executed by the TSGenerator 2.0 core. The historical implementation is archived under legacy/R/. Current ST and QFLAG products should remain separate and be handled with mask_quality() when needed.", call. = FALSE)
}
