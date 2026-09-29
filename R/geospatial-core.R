# Geospatial core ---------------------------------------------------------
# Internal terra-first infrastructure for TSGenerator 2.0.
# Phase 3.1 intentionally does not replace the public 1.x extraction API.

.tsg_require_terra <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required by the TSGenerator 2.0 geospatial core.", call. = FALSE)
  }
  invisible(TRUE)
}

.tsg_validate_vector <- function(x, id_col = NULL) {
  .tsg_require_terra()
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    x <- terra::vect(x)
  }
  if (!inherits(x, "SpatVector")) {
    stop("'polygons' must be an sf/sfc object or a terra SpatVector.", call. = FALSE)
  }
  if (terra::geomtype(x) != "polygons") {
    stop("'polygons' must contain polygon geometries.", call. = FALSE)
  }
  if (nrow(x) < 1L) stop("'polygons' contains no features.", call. = FALSE)

  attrs <- names(x)
  if (!is.null(id_col)) {
    if (!is.character(id_col) || length(id_col) != 1L || !nzchar(id_col)) {
      stop("'id_col' must be NULL or a single non-empty column name.", call. = FALSE)
    }
    if (!id_col %in% attrs) stop(sprintf("ID column '%s' was not found in polygons.", id_col), call. = FALSE)
    ids <- x[[id_col]][, 1]
    if (anyNA(ids) || anyDuplicated(ids)) stop("The selected ID column must contain unique, non-missing values.", call. = FALSE)
  } else {
    ids <- seq_len(nrow(x))
  }
  list(vector = x, ids = ids, id_col = id_col %||% ".feature_id")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.tsg_list_rasters <- function(x, recursive = FALSE) {
  if (inherits(x, "SpatRaster")) return(x)
  if (!is.character(x) || length(x) < 1L) {
    stop("Raster input must be a terra SpatRaster, a TIFF file, a directory, or a character vector of TIFF files.", call. = FALSE)
  }
  if (length(x) == 1L && dir.exists(x)) {
    x <- list.files(x, pattern = "\\.(tif|tiff)$", full.names = TRUE,
                    ignore.case = TRUE, recursive = recursive)
  }
  if (!length(x)) stop("No TIFF rasters were found.", call. = FALSE)
  missing <- x[!file.exists(x)]
  if (length(missing)) stop(sprintf("Raster file does not exist: %s", missing[1]), call. = FALSE)
  ext <- tolower(tools::file_ext(x))
  if (any(!ext %in% c("tif", "tiff"))) stop("All raster files must be TIFF files with .tif or .tiff extensions.", call. = FALSE)
  normalizePath(x, winslash = "/", mustWork = TRUE)
}

.tsg_read_raster <- function(x) {
  .tsg_require_terra()
  if (inherits(x, "SpatRaster")) return(x)
  terra::rast(.tsg_list_rasters(x))
}

.tsg_align_vector_crs <- function(polygons, raster, transform = TRUE) {
  .tsg_require_terra()
  if (!inherits(polygons, "SpatVector") || !inherits(raster, "SpatRaster")) {
    stop("Internal CRS alignment requires SpatVector and SpatRaster objects.", call. = FALSE)
  }
  cr <- terra::crs(raster, proj = TRUE)
  cv <- terra::crs(polygons, proj = TRUE)
  if (!nzchar(cr)) stop("Raster CRS is missing; extraction would be spatially ambiguous.", call. = FALSE)
  if (!nzchar(cv)) stop("Polygon CRS is missing; extraction would be spatially ambiguous.", call. = FALSE)
  same <- terra::same.crs(polygons, raster)
  if (!same && !isTRUE(transform)) stop("Raster and polygon CRS differ. Set transform = TRUE or transform the vector data first.", call. = FALSE)
  if (!same) polygons <- terra::project(polygons, raster)
  polygons
}

.tsg_match_summary_fun <- function(fun) {
  if (is.function(fun)) return(fun)
  if (!is.character(fun) || length(fun) != 1L) stop("'fun' must be a function or one of: mean, median, min, max, sum.", call. = FALSE)
  fun <- tolower(fun)
  allowed <- c("mean", "median", "min", "max", "sum")
  if (!fun %in% allowed) stop(sprintf("Unsupported summary function '%s'.", fun), call. = FALSE)
  switch(fun, mean = mean, median = stats::median, min = min, max = max, sum = sum)
}

.tsg_validate_scale <- function(scale_factor = 1, offset = 0) {
  if (!is.numeric(scale_factor) || length(scale_factor) != 1L || !is.finite(scale_factor) || scale_factor == 0) {
    stop("'scale_factor' must be one finite, non-zero numeric value.", call. = FALSE)
  }
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
    stop("'offset' must be one finite numeric value.", call. = FALSE)
  }
  list(scale_factor = scale_factor, offset = offset)
}

#' Inspect inputs for the TSGenerator 2.0 geospatial engine
#'
#' Validates raster and polygon inputs without extracting values. It reports
#' geometry, CRS compatibility, feature IDs, and the scaling policy that will
#' be used by the terra-first extraction engine introduced in TSGenerator 2.0.
#'
#' @param x A `terra::SpatRaster`, TIFF path, directory containing TIFF files,
#'   or character vector of TIFF paths.
#' @param polygons An `sf`/`sfc` object or `terra::SpatVector` with polygons.
#' @param id_col Optional polygon attribute containing unique feature IDs.
#' @param transform Logical; allow polygons to be projected to the raster CRS.
#' @param scale_factor Numeric divisor applied after extraction. Default 1.
#' @param offset Numeric offset applied after scaling. Default 0.
#' @return An object of class `tsg_geospatial_plan`. No raster values are loaded
#'   into memory and no extraction is performed.
#' @family geospatial core
#' @export
geospatial_plan <- function(x, polygons, id_col = NULL, transform = TRUE,
                            scale_factor = 1, offset = 0) {
  .tsg_require_terra()
  vr <- .tsg_validate_vector(polygons, id_col)
  rr <- .tsg_read_raster(x)
  vv <- .tsg_align_vector_crs(vr$vector, rr, transform = transform)
  sc <- .tsg_validate_scale(scale_factor, offset)
  structure(list(
    raster = rr,
    polygons = vv,
    ids = vr$ids,
    id_col = vr$id_col,
    n_features = nrow(vv),
    n_layers = terra::nlyr(rr),
    raster_crs = terra::crs(rr, proj = TRUE),
    vector_crs = terra::crs(vv, proj = TRUE),
    transformed = !terra::same.crs(vr$vector, vv),
    scale_factor = sc$scale_factor,
    offset = sc$offset
  ), class = "tsg_geospatial_plan")
}

#' @family geospatial core
#' @export
print.tsg_geospatial_plan <- function(x, ...) {
  cat("TSGenerator geospatial plan\n")
  cat(sprintf("  Raster layers: %d\n", x$n_layers))
  cat(sprintf("  Polygon features: %d\n", x$n_features))
  cat(sprintf("  ID field: %s\n", x$id_col))
  cat(sprintf("  CRS aligned: %s\n", if (terra::same.crs(x$polygons, x$raster)) "yes" else "no"))
  cat(sprintf("  Scale factor: %s; offset: %s\n", x$scale_factor, x$offset))
  invisible(x)
}
