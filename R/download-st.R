# Seasonal Trajectories acquisition --------------------------------------

.ST_DATASET_ID <- "EO:EEA:DAT:CLMS_HRVPP_ST"
.ST_MAX_WINDOW_DAYS <- 31L

#' Download Copernicus HR-VPP Seasonal Trajectories
#'
#' Search and optionally download the currently supported Copernicus
#' HR-VPP Seasonal Trajectories (ST) products from WEkEO using the native
#' R HDA backend. The function supports PPI, QFLAG, or both products and
#' automatically splits long requests into windows of at most 31 days.
#'
#' @param start Start date (`Date`, `POSIXt`, or ISO character string).
#' @param end End date (`Date`, `POSIXt`, or ISO character string).
#' @param output_dir Directory where files are downloaded.
#' @param product Character vector containing `"PPI"`, `"QFLAG"`, or both.
#' @param tile_id Optional Sentinel-2 tile identifier, e.g. `"30TXM"`.
#' @param bbox Optional numeric vector `c(xmin, ymin, xmax, ymax)` in
#'   longitude/latitude (EPSG:4326).
#' @param platform Platform filter. One of `"S2A, S2B"`, `"S2A"`, `"S2B"`,
#'   or `NULL` to omit the filter.
#' @param resolution Spatial resolution in metres. ST currently supports 10 m.
#' @param product_version Optional HR-VPP product version filter.
#' @param client Optional client created by [hda_client()].
#' @param download Logical. If `FALSE`, only search/preview results.
#' @param limit Optional maximum number of search results per query.
#' @param overwrite Logical; passed to `hdar` as `force`. Existing files are
#'   skipped by default.
#' @param prompt Logical; allow `hdar` to ask for confirmation before downloading.
#'   Defaults to [interactive()], so scripts and Shiny sessions are non-interactive.
#' @param quiet Logical; suppress progress messages from TSGenerator.
#' @param retries Number of retries after transient WEkEO search/download failures.
#' @param backoff Initial retry delay in seconds; exponential backoff is used.
#'
#' @return An object of class `tsg_st_download`, containing the submitted
#'   queries, WEkEO search results, request summary, output directory and
#'   download status. Search result objects are retained for reproducibility.
#' @family WEkEO acquisition
#' @export
#'
#' @examples
#' \dontrun{
#' # Preview one month of PPI for a Sentinel-2 tile
#' x <- download_st(
#'   start = "2020-04-01", end = "2020-04-30",
#'   tile_id = "30TXM", download = FALSE
#' )
#'
#' # Download PPI and the corresponding ST quality flag
#' x <- download_st(
#'   start = "2020-04-01", end = "2020-06-30",
#'   tile_id = "30TXM", product = c("PPI", "QFLAG"),
#'   output_dir = "HRVPP_ST"
#' )
#' }
download_st <- function(start, end,
                        output_dir = "HRVPP_ST",
                        product = "PPI",
                        tile_id = NULL,
                        bbox = NULL,
                        platform = "S2A, S2B",
                        resolution = 10L,
                        product_version = NULL,
                        client = NULL,
                        download = TRUE,
                        limit = NULL,
                        overwrite = FALSE,
                        prompt = interactive(),
                        quiet = FALSE,
                        retries = 2L,
                        backoff = 2) {
  .st_require_backend()
  dates <- .st_validate_dates(start, end)
  product <- .st_validate_product(product)
  tile_id <- .st_validate_tile(tile_id)
  bbox <- .st_validate_bbox(bbox)
  platform <- .st_validate_platform(platform)

  if (!is.numeric(resolution) || length(resolution) != 1L || is.na(resolution) || resolution != 10) {
    stop("'resolution' must be 10 for the 10 m ST dataset.", call. = FALSE)
  }
  if (!is.null(product_version) &&
      (!is.character(product_version) || length(product_version) != 1L || !nzchar(product_version))) {
    stop("'product_version' must be NULL or one non-empty character string.", call. = FALSE)
  }
  if (!is.logical(download) || length(download) != 1L || is.na(download)) {
    stop("'download' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(prompt) || length(prompt) != 1L || is.na(prompt)) {
    stop("'prompt' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(limit) && (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit < 1)) {
    stop("'limit' must be NULL or a positive number.", call. = FALSE)
  }

  if (is.null(client)) client <- hda_client()
  windows <- .st_date_windows(dates$start, dates$end)
  temporal_fields <- .hda_temporal_fields(.ST_DATASET_ID, client = client)
  requests <- vector("list", length(product) * nrow(windows))
  k <- 0L

  if (isTRUE(download)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) stop("Could not create output directory: ", output_dir, call. = FALSE)
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  }

  for (p in product) {
    for (i in seq_len(nrow(windows))) {
      k <- k + 1L
      q <- .st_build_query(
        product = p,
        start = windows$start[i],
        end = windows$end[i],
        tile_id = tile_id,
        bbox = bbox,
        platform = platform,
        resolution = resolution,
        product_version = product_version,
        temporal_fields = temporal_fields
      )

      if (!isTRUE(quiet)) {
        message("Searching ST ", p, ": ", windows$start[i], " to ", windows$end[i], "...")
      }
      sr <- tryCatch(
        hda_search(q$json, client = client, limit = limit, retries = retries, backoff = backoff),
        error = function(e) stop(
          "WEkEO ST search failed for ", p, " (", windows$start[i], " to ",
          windows$end[i], "): ", conditionMessage(e), call. = FALSE
        )
      )

      meta <- .st_result_metadata(sr)
      n_found <- .st_result_count(sr)
      skipped <- character()
      selected_indexes <- NULL
      n_to_download <- n_found
      if (isTRUE(download) && !isTRUE(overwrite) && nrow(meta) > 0L) {
        keep <- !vapply(meta$id, .st_product_exists, logical(1), output_dir = output_dir)
        skipped <- meta$id[!keep]
        selected_indexes <- which(keep)
        n_to_download <- length(selected_indexes)
      }

      status <- if (!isTRUE(download)) "preview" else if (n_to_download == 0L) "nothing_to_download" else "downloaded"
      if (isTRUE(download) && n_to_download > 0L) {
        if (!isTRUE(quiet)) message("Downloading ", n_to_download, " ST item(s)...")
        .tsg_retry(function() {
          if (is.null(selected_indexes)) {
            sr$download(output_dir, force = isTRUE(overwrite), prompt = isTRUE(prompt))
          } else {
            sr$download(output_dir, selected_indexes = selected_indexes,
                        force = isTRUE(overwrite), prompt = isTRUE(prompt))
          }
        }, retries = retries, backoff = backoff, label = "WEkEO ST download")
      }

      requests[[k]] <- list(
        product = p,
        start = windows$start[i],
        end = windows$end[i],
        query = q$list,
        query_json = q$json,
        results = sr,
        metadata = meta,
        n_results = .st_result_count(sr),
        skipped_existing = skipped,
        bytes_found = .tsg_bytes(meta),
        bytes_to_download = .tsg_bytes(meta, selected_indexes),
        status = status
      )
    }
  }

  summary <- do.call(rbind, lapply(requests, function(z) {
    data.frame(
      product = z$product,
      start = as.character(z$start),
      end = as.character(z$end),
      n_results = z$n_results,
      skipped_existing = length(z$skipped_existing),
      size_found = .tsg_human_size(z$bytes_found),
      size_to_download = .tsg_human_size(z$bytes_to_download),
      status = z$status,
      stringsAsFactors = FALSE
    )
  }))

  out <- list(
    dataset_id = .ST_DATASET_ID,
    products = product,
    requested_start = dates$start,
    requested_end = dates$end,
    tile_id = tile_id,
    bbox = bbox,
    output_dir = if (isTRUE(download)) output_dir else NULL,
    downloaded = isTRUE(download),
    requests = requests,
    summary = summary
  )
  class(out) <- "tsg_st_download"
  out
}

#' @family WEkEO acquisition
#' @export
print.tsg_st_download <- function(x, ...) {
  cat("TSGenerator HR-VPP Seasonal Trajectories request\n")
  cat("Dataset: ", x$dataset_id, "\n", sep = "")
  cat("Period:  ", as.character(x$requested_start), " to ", as.character(x$requested_end), "\n", sep = "")
  cat("Products: ", paste(x$products, collapse = ", "), "\n", sep = "")
  if (!is.null(x$tile_id)) cat("Tile:     ", x$tile_id, "\n", sep = "")
  cat("Mode:     ", if (isTRUE(x$downloaded)) "download" else "preview", "\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

.st_require_backend <- function() {
  if (!requireNamespace("hdar", quietly = TRUE)) {
    stop("Package 'hdar' is required. Install it with install.packages('hdar').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').", call. = FALSE)
  }
  invisible(TRUE)
}

.st_validate_dates <- function(start, end) {
  to_date <- function(x, arg) {
    if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
    if (inherits(x, "Date")) return(x)
    if (is.character(x) && length(x) == 1L && nzchar(x)) {
      y <- suppressWarnings(as.Date(substr(x, 1L, 10L)))
      if (!is.na(y)) return(y)
    }
    stop("'", arg, "' must be a valid Date, POSIXt, or ISO date string.", call. = FALSE)
  }
  s <- to_date(start, "start")
  e <- to_date(end, "end")
  if (s > e) stop("'start' must be earlier than or equal to 'end'.", call. = FALSE)
  list(start = s, end = e)
}

.st_validate_product <- function(product) {
  if (!is.character(product) || length(product) < 1L || anyNA(product)) {
    stop("'product' must contain 'PPI', 'QFLAG', or both.", call. = FALSE)
  }
  product <- unique(toupper(product))
  bad <- setdiff(product, c("PPI", "QFLAG"))
  if (length(bad)) stop("Unsupported ST product(s): ", paste(bad, collapse = ", "), ".", call. = FALSE)
  product
}

.st_validate_tile <- function(tile_id) {
  if (is.null(tile_id)) return(NULL)
  if (!is.character(tile_id) || length(tile_id) != 1L || !nzchar(tile_id)) {
    stop("'tile_id' must be NULL or one Sentinel-2 tile identifier.", call. = FALSE)
  }
  tile_id <- toupper(trimws(tile_id))
  tile_id <- sub("^T", "", tile_id)
  if (!grepl("^[0-9]{2}[A-Z]{3}$", tile_id)) {
    stop("'tile_id' must look like a Sentinel-2 MGRS tile, e.g. '30TXM'.", call. = FALSE)
  }
  tile_id
}

.st_validate_bbox <- function(bbox) {
  if (is.null(bbox)) return(NULL)
  if (!is.numeric(bbox) || length(bbox) != 4L || anyNA(bbox) || any(!is.finite(bbox))) {
    stop("'bbox' must be c(xmin, ymin, xmax, ymax).", call. = FALSE)
  }
  if (bbox[1] < -180 || bbox[3] > 180 || bbox[2] < -90 || bbox[4] > 90 ||
      bbox[1] >= bbox[3] || bbox[2] >= bbox[4]) {
    stop("'bbox' must contain valid EPSG:4326 bounds in xmin, ymin, xmax, ymax order.", call. = FALSE)
  }
  as.numeric(bbox)
}

.st_validate_platform <- function(platform) {
  if (is.null(platform)) return(NULL)
  if (!is.character(platform) || length(platform) != 1L || !platform %in% c("S2A, S2B", "S2A", "S2B")) {
    stop("'platform' must be NULL, 'S2A, S2B', 'S2A', or 'S2B'.", call. = FALSE)
  }
  platform
}

.st_date_windows <- function(start, end, max_days = .ST_MAX_WINDOW_DAYS) {
  starts <- as.Date(character())
  ends <- as.Date(character())
  cur <- start
  while (cur <= end) {
    # inclusive window: max_days calendar days
    this_end <- min(end, cur + (max_days - 1L))
    starts <- c(starts, cur)
    ends <- c(ends, this_end)
    cur <- this_end + 1L
  }
  data.frame(start = starts, end = ends)
}

.st_iso_start <- function(x) paste0(as.character(x), "T00:00:00.000Z")
.st_iso_end <- function(x) paste0(as.character(x), "T23:59:59.999Z")

.st_build_query <- function(product, start, end, tile_id, bbox, platform,
                            resolution, product_version,
                            temporal_fields = c(start = "start", end = "end")) {
  q <- list(
    dataset_id = .ST_DATASET_ID,
    productType = product,
    resolution = as.character(as.integer(resolution)),
    itemsPerPage = 200L,
    startIndex = 0L
  )
  q[[unname(temporal_fields[["start"]])]] <- .st_iso_start(start)
  q[[unname(temporal_fields[["end"]])]] <- .st_iso_end(end)
  if (!is.null(platform)) q$platformSerialIdentifier <- platform
  if (!is.null(tile_id)) q$tileId <- tile_id
  if (!is.null(product_version)) q$productVersion <- product_version
  if (!is.null(bbox)) q$bbox <- bbox
  json <- jsonlite::toJSON(q, auto_unbox = TRUE, digits = NA)
  list(list = q, json = json)
}

.st_result_count <- function(results) {
  if (is.null(results)) return(0L)
  # hdar SearchResults currently exposes an R6 `results` member.
  n <- tryCatch(length(results$results), error = function(e) NA_integer_)
  if (is.na(n)) n <- tryCatch(length(results), error = function(e) NA_integer_)
  if (is.na(n)) 0L else as.integer(n)
}

.st_result_metadata <- function(results) {
  rr <- tryCatch(results$results, error = function(e) NULL)
  if (is.null(rr) || length(rr) == 0L) {
    return(data.frame(id = character(), location = character(), size = numeric(), stringsAsFactors = FALSE))
  }
  rows <- lapply(rr, function(z) {
    prop <- z$properties
    data.frame(
      id = if (!is.null(z$id)) as.character(z$id) else NA_character_,
      location = if (!is.null(prop$location)) as.character(prop$location) else NA_character_,
      size = if (!is.null(prop$size) && is.numeric(prop$size)) as.numeric(prop$size) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.st_product_exists <- function(id, output_dir) {
  if (is.na(id) || !nzchar(id)) return(FALSE)
  files <- list.files(output_dir, recursive = TRUE, full.names = FALSE)
  any(startsWith(basename(files), id))
}
