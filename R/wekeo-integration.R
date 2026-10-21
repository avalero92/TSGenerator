# Live WEkEO integration diagnostics -------------------------------------

#' Run a live WEkEO integration smoke test
#'
#' Performs a small, non-destructive end-to-end check of the TSGenerator
#' acquisition layer against the live WEkEO service. By default it performs
#' authentication, live schema discovery and preview searches only. Downloads
#' are opt-in because HR-VPP GeoTIFFs can be large.
#'
#' The function deliberately resolves the temporal field names from WEkEO's
#' live `/queryable` schema. This protects TSGenerator from provider-side
#' transitions between fields such as `start`/`end` and
#' `startdate`/`enddate`.
#'
#' @param client Optional client created by [hda_client()].
#' @param st_tile Sentinel-2 MGRS tile used for the ST smoke test.
#' @param vpp_tile Sentinel-2 MGRS tile used for the VPP smoke test.
#' @param st_start,st_end Small ST date range.
#' @param vpp_start,vpp_end VPP date range.
#' @param vpp_product VPP parameter used for the smoke test.
#' @param vpp_season VPP season (`"s1"` or `"s2"`).
#' @param download Logical. If `FALSE` (default), searches only. If `TRUE`,
#'   matching files are downloaded to `output_dir`.
#' @param output_dir Explicit destination directory required when `download = TRUE`.
#'   No destination is selected or created by default.
#' @param quiet Logical; suppress progress messages.
#' @return A list of class `tsg_wekeo_integration` containing diagnostics,
#'   live temporal field names, ST/VPP results and an overall status.
#' @family WEkEO acquisition
#' @export
check_wekeo_integration <- function(
    client = NULL,
    st_tile = "32TPS",
    vpp_tile = "30TXR",
    st_start = "2018-03-01",
    st_end = "2018-03-11",
    vpp_start = "2018-01-01",
    vpp_end = "2018-12-31",
    vpp_product = "TPROD",
    vpp_season = "s1",
    download = FALSE,
    output_dir = NULL,
    quiet = FALSE) {
  if (!is.logical(download) || length(download) != 1L || is.na(download)) {
    stop("'download' must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(download) &&
      (!is.character(output_dir) || length(output_dir) != 1L ||
       is.na(output_dir) || !nzchar(output_dir))) {
    stop("'output_dir' must be explicitly specified when 'download = TRUE'.",
         call. = FALSE)
  }
  if (is.null(client)) client <- hda_client()

  diag <- check_wekeo(client = client, online = TRUE)
  st_fields <- .hda_temporal_fields(.ST_DATASET_ID, client)
  vpp_fields <- .hda_temporal_fields(.VPP_DATASET_ID, client)

  st_dir <- if (isTRUE(download)) file.path(output_dir, "ST") else NULL
  vpp_dir <- if (isTRUE(download)) file.path(output_dir, "VPP")
  st <- tryCatch(download_st(
    start = st_start, end = st_end, tile_id = st_tile,
    product = "PPI", output_dir = st_dir, client = client,
    download = download, prompt = FALSE, quiet = quiet, retries = 1L
  ), error = function(e) e)
  vpp <- tryCatch(download_vpp(
    start = vpp_start, end = vpp_end, tile_id = vpp_tile,
    product = vpp_product, season = vpp_season,
    output_dir = vpp_dir, client = client,
    download = download, prompt = FALSE, quiet = quiet, retries = 1L
  ), error = function(e) e)

  st_ok <- !inherits(st, "error") && sum(st$summary$n_results) > 0L
  vpp_ok <- !inherits(vpp, "error") && sum(vpp$summary$n_results) > 0L

  files <- character()
  raster_ok <- NA
  if (isTRUE(download)) {
    files <- list.files(output_dir, pattern = "\\.tif(f)?$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    raster_ok <- length(files) > 0L && all(vapply(files, function(f) {
      tryCatch({ r <- terra::rast(f); terra::nlyr(r) >= 1L }, error = function(e) FALSE)
    }, logical(1)))
  }

  out <- list(
    diagnostics = diag,
    temporal_fields = list(ST = st_fields, VPP = vpp_fields),
    ST = st,
    VPP = vpp,
    downloaded_files = files,
    raster_readable = raster_ok,
    ok = isTRUE(st_ok) && isTRUE(vpp_ok) && (!isTRUE(download) || isTRUE(raster_ok)),
    mode = if (isTRUE(download)) "download" else "preview"
  )
  class(out) <- "tsg_wekeo_integration"
  out
}

#' @export
print.tsg_wekeo_integration <- function(x, ...) {
  cat("TSGenerator live WEkEO integration check\n")
  cat("Mode:   ", x$mode, "\n", sep = "")
  cat("ST time fields:  ", paste(unname(x$temporal_fields$ST), collapse = " / "), "\n", sep = "")
  cat("VPP time fields: ", paste(unname(x$temporal_fields$VPP), collapse = " / "), "\n", sep = "")
  cat("ST:     ", if (inherits(x$ST, "error")) conditionMessage(x$ST) else paste0(sum(x$ST$summary$n_results), " result(s)"), "\n", sep = "")
  cat("VPP:    ", if (inherits(x$VPP, "error")) conditionMessage(x$VPP) else paste0(sum(x$VPP$summary$n_results), " result(s)"), "\n", sep = "")
  if (identical(x$mode, "download")) cat("GeoTIFF readable: ", isTRUE(x$raster_readable), "\n", sep = "")
  cat("Overall: ", if (isTRUE(x$ok)) "PASS" else "FAIL", "\n", sep = "")
  invisible(x)
}
