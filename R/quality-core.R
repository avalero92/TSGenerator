# HR-VPP quality infrastructure --------------------------------------------

.tsg_quality_specs <- function(type = c("ST", "VPP")) {
  type <- match.arg(toupper(type), c("ST", "VPP"))
  if (type == "ST") {
    return(data.frame(
      Code = 0:5,
      Class = c("No data", "Filled - extrapolation", "Filled - interpolation",
                "Low", "Medium", "High"),
      Rank = 0:5,
      AcceptDefault = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
      Definition = c(
        "Time series was not processed",
        "Clear-sky observation on one side outside the 91-day window",
        "Clear-sky observations on both sides outside the 91-day window",
        "1-2 clear-sky land observations in the 91-day window",
        "3-8 clear-sky land observations in the 91-day window",
        "More than 8 clear-sky land observations in the 91-day window"
      ), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Code = 0:10,
    Class = c("No data", "No season found", "Filled", "Reserved",
              "Low", "Low", "Low", "Medium", "Medium", "Medium", "High"),
    Rank = c(0,0,1,NA,2,2,2,3,3,3,4),
    AcceptDefault = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE),
    Definition = c(
      "Time series was not processed",
      "No season found although the time series was processed",
      "Insufficient clear-sky observations in green-up, green-down and green-peak",
      "Reserved",
      "More than 2 clear-sky observations during green-down",
      "More than 2 clear-sky observations during green-peak",
      "More than 2 clear-sky observations during green-up",
      "More than 2 clear-sky observations during green-up and green-peak",
      "More than 2 clear-sky observations during green-up and green-down",
      "More than 2 clear-sky observations during green-peak and green-down",
      "More than 2 clear-sky observations during green-up, green-down and green-peak"
    ), stringsAsFactors = FALSE
  )
}

#' HR-VPP quality flag definitions
#'
#' Returns the quality-code table used by TSGenerator for Seasonal
#' Trajectories (ST) or Vegetation Phenology and Productivity (VPP).
#' The default acceptance policy is deliberately explicit: ST codes 3-5 and
#' VPP codes 7-10 are considered acceptable. Users can override thresholds in
#' masking functions.
#' @param type Either `"ST"` or `"VPP"`.
#' @return A data frame describing quality codes, classes and definitions.
#' @family product quality
#' @export
quality_info <- function(type = c("ST", "VPP")) {
  .tsg_quality_specs(type)
}

.tsg_validate_quality_codes <- function(values, type) {
  specs <- .tsg_quality_specs(type)
  x <- suppressWarnings(as.integer(values))
  bad <- !is.na(x) & !x %in% specs$Code
  if (any(bad)) warning(sprintf("%d quality value(s) are outside the documented %s code range and were classified as Unknown.", sum(bad), toupper(type)), call. = FALSE)
  x
}

#' Decode HR-VPP quality codes
#'
#' @param values Numeric/integer quality codes.
#' @param type `"ST"` or `"VPP"`.
#' @return A data frame with code, quality class, rank, default acceptance and definition.
#' @family product quality
#' @export
classify_quality <- function(values, type = c("ST", "VPP")) {
  type <- match.arg(toupper(type), c("ST", "VPP"))
  code <- .tsg_validate_quality_codes(values, type)
  specs <- .tsg_quality_specs(type)
  idx <- match(code, specs$Code)
  out <- data.frame(
    Code = code,
    Class = specs$Class[idx],
    Rank = specs$Rank[idx],
    Accepted = specs$AcceptDefault[idx],
    Definition = specs$Definition[idx],
    stringsAsFactors = FALSE
  )
  unknown <- !is.na(code) & is.na(idx)
  out$Class[unknown] <- "Unknown"
  out$Accepted[unknown] <- FALSE
  out$Definition[unknown] <- "Undocumented quality code"
  out
}

#' Summarize HR-VPP quality flags
#'
#' Accepts a numeric vector, a quality `SpatRaster`, or a data frame containing
#' a quality-code column. No binary masking is performed.
#' @param x Quality codes, a `terra::SpatRaster`, or a data frame.
#' @param type `"ST"` or `"VPP"`.
#' @param quality_col Column containing codes when `x` is a data frame.
#' @return A data frame with counts and percentages by documented quality code.
#' @family product quality
#' @export
summarize_quality <- function(x, type = c("ST", "VPP"), quality_col = "QFLAG") {
  type <- match.arg(toupper(type), c("ST", "VPP"))
  if (inherits(x, "SpatRaster")) {
    .tsg_require_terra()
    f <- terra::freq(x, value = TRUE, useNA = "ifany")
    if (is.list(f)) f <- do.call(rbind, f)
    vals <- rep(f$value, f$count)
  } else if (is.data.frame(x)) {
    if (!quality_col %in% names(x)) stop(sprintf("Column '%s' was not found.", quality_col), call. = FALSE)
    vals <- x[[quality_col]]
  } else vals <- x
  dec <- classify_quality(vals, type)
  tab <- as.data.frame(table(Code = dec$Code, useNA = "ifany"), stringsAsFactors = FALSE)
  tab$Code <- suppressWarnings(as.integer(as.character(tab$Code)))
  specs <- .tsg_quality_specs(type)
  tab$Class <- specs$Class[match(tab$Code, specs$Code)]
  tab$Accepted <- specs$AcceptDefault[match(tab$Code, specs$Code)]
  tab$Percent <- if (sum(tab$Freq) > 0) 100 * tab$Freq / sum(tab$Freq) else 0
  tab[, c("Code", "Class", "Accepted", "Freq", "Percent")]
}

.tsg_quality_keep_codes <- function(type, min_quality = NULL, keep_codes = NULL) {
  specs <- .tsg_quality_specs(type)
  if (!is.null(keep_codes)) {
    keep_codes <- unique(as.integer(keep_codes))
    if (anyNA(keep_codes) || any(!keep_codes %in% specs$Code)) stop("'keep_codes' contains undocumented quality codes.", call. = FALSE)
    return(keep_codes)
  }
  if (is.null(min_quality)) return(specs$Code[specs$AcceptDefault])
  if (length(min_quality) != 1L || is.na(min_quality)) stop("'min_quality' must be one documented code or NULL.", call. = FALSE)
  min_quality <- as.integer(min_quality)
  if (!min_quality %in% specs$Code) stop("'min_quality' is outside the documented quality-code range.", call. = FALSE)
  # Numeric thresholds are safe for the documented ST confidence scale. For VPP
  # codes 7-10 form the ordered medium/high group used for threshold masking.
  specs$Code[specs$Code >= min_quality]
}

#' Mask an HR-VPP raster using its quality flag
#'
#' Masks a target raster without resampling quality flags. Target and quality
#' rasters must already have identical geometry, preventing categorical QFLAG
#' interpolation. By default ST keeps codes 3-5 and VPP keeps codes 7-10.
#' @param x Target `terra::SpatRaster`.
#' @param quality Quality `terra::SpatRaster` with matching geometry/layers.
#' @param type `"ST"` or `"VPP"`.
#' @param min_quality Optional minimum code. Defaults to the documented policy
#'   used by TSGenerator (ST >=3; VPP >=7).
#' @param keep_codes Optional explicit codes to retain; overrides `min_quality`.
#' @return A masked `terra::SpatRaster`.
#' @family product quality
#' @export
mask_quality <- function(x, quality, type = c("ST", "VPP"), min_quality = NULL, keep_codes = NULL) {
  .tsg_require_terra()
  type <- match.arg(toupper(type), c("ST", "VPP"))
  if (!inherits(x, "SpatRaster") || !inherits(quality, "SpatRaster")) stop("'x' and 'quality' must be terra::SpatRaster objects.", call. = FALSE)
  if (!isTRUE(terra::compareGeom(x, quality, stopOnError = FALSE, crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE))) {
    stop("Target and quality rasters do not have identical geometry. QFLAG rasters are categorical and TSGenerator will not resample them automatically.", call. = FALSE)
  }
  if (!(terra::nlyr(quality) %in% c(1L, terra::nlyr(x)))) stop("'quality' must have one layer or the same number of layers as 'x'.", call. = FALSE)
  keep <- .tsg_quality_keep_codes(type, min_quality, keep_codes)
  q <- quality
  if (terra::nlyr(q) == 1L && terra::nlyr(x) > 1L) q <- q[[rep(1L, terra::nlyr(x))]]
  names(q) <- names(x)
  # `%in%` dispatches through base::match() and is not defined for SpatRaster.
  # Evaluate code membership block-wise with terra::app(), preserving raster
  # geometry and avoiding any resampling of categorical QFLAG values.
  accepted <- terra::app(q, fun = function(v) v %in% keep)
  names(accepted) <- names(x)
  terra::ifel(accepted, x, NA)
}
