#' Extract polygon-level raster time series
#'
#' Extracts one summary value per polygon and raster layer using the terra-first
#' TSGenerator 2.0 geospatial engine. This function replaces the duplicated
#' `get.Series.mean()` and `get.Series.median()` workflows for new code.
#'
#' @param x A `terra::SpatRaster`, TIFF path, directory containing TIFF files,
#'   or character vector of TIFF paths.
#' @param polygons An `sf`/`sfc` object or `terra::SpatVector` of polygons.
#' @param id_col Optional polygon attribute containing unique IDs. If `NULL`,
#'   stable feature IDs `1:n` are generated for the returned table.
#' @param fun Summary function: `"median"`, `"mean"`, `"min"`, `"max"`,
#'   `"sum"`, or a function accepted by `terra::extract()`.
#' @param dates Optional vector of dates, one per raster layer. If omitted,
#'   TSGenerator first uses a complete `terra::time()` vector and otherwise
#'   attempts to parse YYYY-MM-DD or YYYYMMDD from source/layer names.
#' @param transform Logical; if `TRUE`, polygons are transformed to the raster
#'   CRS when needed. The raster grid is never reprojected by this function.
#' @param scale_factor Numeric divisor applied to extracted values. Default 1.
#' @param offset Numeric offset added after division. Default 0. The resulting
#'   value is `raw / scale_factor + offset`.
#' @param na.rm Logical passed to the summary function by `terra::extract()`.
#' @param exact Logical; if `TRUE`, use exact polygon-cell fractions when
#'   supported by `terra::extract()`. Default `FALSE`.
#' @param touches Logical; include cells touched by polygon boundaries. Default
#'   `FALSE`; ignored by terra when incompatible with the selected mode.
#' @return A tidy data frame with columns `ID`, `Date`, and `Value`, plus
#'   `Layer` when layer names are needed for traceability. The result also has
#'   class `tsg_time_series`.
#' @family geospatial core
#' @export
extract_ts <- function(x, polygons, id_col = NULL, fun = "median", dates = NULL,
                       transform = TRUE, scale_factor = 1, offset = 0,
                       na.rm = TRUE, exact = FALSE, touches = FALSE) {
  .tsg_require_terra()
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) stop("'na.rm' must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(exact) || length(exact) != 1L || is.na(exact)) stop("'exact' must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(touches) || length(touches) != 1L || is.na(touches)) stop("'touches' must be TRUE or FALSE.", call. = FALSE)

  plan <- geospatial_plan(x, polygons, id_col = id_col, transform = transform,
                          scale_factor = scale_factor, offset = offset)
  summary_fun <- .tsg_match_summary_fun(fun)
  layer_dates <- .tsg_resolve_layer_dates(x, plan$raster, dates)
  layer_names <- names(plan$raster)
  if (length(layer_names) != plan$n_layers) layer_names <- paste0("layer_", seq_len(plan$n_layers))

  z <- terra::extract(plan$raster, plan$polygons, fun = summary_fun,
                      na.rm = na.rm, exact = exact, touches = touches)
  z <- as.data.frame(z)
  if (nrow(z) != plan$n_features) {
    stop("Unexpected extraction result: number of rows does not match polygon features.", call. = FALSE)
  }
  # terra::extract() returns one leading feature-ID column followed by one
  # value column per raster layer. Select value columns by position rather
  # than by name: HR-VPP GeoTIFFs can legitimately expose identical band
  # descriptions (for example, repeated PPI layer names), and name-based
  # set operations collapse duplicates.
  expected_cols <- plan$n_layers + 1L
  if (ncol(z) != expected_cols) {
    stop(
      sprintf(
        "Unexpected extraction result: expected 1 feature-ID column + %d raster value column(s), got %d column(s).",
        plan$n_layers, ncol(z)
      ),
      call. = FALSE
    )
  }
  value_idx <- seq.int(2L, ncol(z))
  values <- as.matrix(z[, value_idx, drop = FALSE])
  storage.mode(values) <- "double"
  values <- values / plan$scale_factor + plan$offset

  out <- data.frame(
    ID = rep(plan$ids, times = plan$n_layers),
    Date = rep(layer_dates, each = plan$n_features),
    Layer = rep(layer_names, each = plan$n_features),
    Value = as.vector(values),
    stringsAsFactors = FALSE
  )
  attr(out, "summary_function") <- if (is.character(fun)) tolower(fun) else "custom"
  attr(out, "scale_factor") <- plan$scale_factor
  attr(out, "offset") <- plan$offset
  attr(out, "raster_crs") <- plan$raster_crs
  class(out) <- c("tsg_time_series", "data.frame")
  out
}

.tsg_resolve_layer_dates <- function(x, raster, dates = NULL) {
  n <- terra::nlyr(raster)
  if (!is.null(dates)) return(.tsg_validate_dates(dates, n))

  tt <- tryCatch(terra::time(raster), error = function(e) NULL)
  if (!is.null(tt) && length(tt) == n && !all(is.na(tt))) {
    if (anyNA(tt)) stop("Raster time metadata is incomplete. Supply 'dates' explicitly.", call. = FALSE)
    return(as.Date(tt))
  }

  candidates <- NULL
  if (is.character(x)) {
    files <- tryCatch(.tsg_list_rasters(x), error = function(e) NULL)
    if (!is.null(files) && length(files) == n) candidates <- basename(files)
  }
  if (is.null(candidates)) candidates <- names(raster)

  parsed <- tryCatch(.extract_dates_from_tiff_files(candidates), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) != n) {
    stop("Could not determine one date per raster layer. Supply 'dates' explicitly or include YYYY-MM-DD/YYYYMMDD in layer/file names.", call. = FALSE)
  }
  .tsg_validate_dates(parsed, n)
}

.tsg_validate_dates <- function(dates, n_layers) {
  if (length(dates) != n_layers) stop(sprintf("'dates' must contain exactly %d value(s), one per raster layer.", n_layers), call. = FALSE)
  if (inherits(dates, "POSIXt")) dates <- as.Date(dates)
  if (!inherits(dates, "Date")) {
    raw <- as.character(dates)
    compact <- grepl("^[0-9]{8}$", raw)
    out <- as.Date(rep(NA_character_, length(raw)))
    if (any(compact)) out[compact] <- as.Date(raw[compact], format = "%Y%m%d")
    if (any(!compact)) {
      parsed <- suppressWarnings(tryCatch(
        as.Date(raw[!compact], format = "%Y-%m-%d"),
        error = function(e) rep(as.Date(NA), sum(!compact))
      ))
      out[!compact] <- parsed
    }
    dates <- out
  }
  if (anyNA(dates)) stop("'dates' contains values that cannot be converted to Date.", call. = FALSE)
  dates
}

#' @family geospatial core
#' @export
print.tsg_time_series <- function(x, ...) {
  cat("TSGenerator extracted time series\n")
  cat(sprintf("  Records: %d\n", nrow(x)))
  cat(sprintf("  Features: %d\n", length(unique(x$ID))))
  cat(sprintf("  Dates: %d\n", length(unique(x$Date))))
  cat(sprintf("  Summary: %s\n", attr(x, "summary_function") %||% "unknown"))
  NextMethod("print", x, ...)
}
