# Vegetation Phenology and Productivity acquisition -----------------------

.VPP_DATASET_ID <- "EO:EEA:DAT:CLMS_HRVPP_VPP"
.VPP_PRODUCTS <- c(
  "MINV", "MAXD", "LENGTH", "SOSD", "QFLAG", "EOSV", "TPROD",
  "MAXV", "AMPL", "SOSV", "LSLOPE", "EOSD", "RSLOPE", "SPROD"
)
.VPP_SEASONS <- c("s1", "s2")

#' Download Copernicus HR-VPP phenology and productivity parameters
#'
#' Search and optionally download Copernicus HR-VPP Vegetation Phenology and
#' Productivity (VPP) products from WEkEO using the native R HDA backend.
#' Requests can include one or several VPP parameters and season groups.
#'
#' @param start Start date (`Date`, `POSIXt`, or ISO character string).
#' @param end End date (`Date`, `POSIXt`, or ISO character string).
#' @param output_dir Directory where files are downloaded.
#' @param product Character vector of VPP product types. Supported values are
#'   `MINV`, `MAXD`, `LENGTH`, `SOSD`, `QFLAG`, `EOSV`, `TPROD`, `MAXV`,
#'   `AMPL`, `SOSV`, `LSLOPE`, `EOSD`, `RSLOPE`, and `SPROD`. Use `"all"`
#'   to request every supported parameter.
#' @param season Character vector containing `"s1"`, `"s2"`, or both.
#'   Set to `NULL` to omit the `productGroupId` filter.
#' @param tile_id Optional Sentinel-2 tile identifier, e.g. `"30TXM"`.
#' @param bbox Optional numeric vector `c(xmin, ymin, xmax, ymax)` in
#'   longitude/latitude (EPSG:4326).
#' @param platform Platform filter. The current VPP dataset identifies the
#'   combined Sentinel-2A/Sentinel-2B source as `"S2A, S2B"`. Set to `NULL`
#'   to omit this filter.
#' @param product_version Optional HR-VPP product version filter.
#' @param client Optional client created by [hda_client()].
#' @param download Logical. If `FALSE`, search/preview results only.
#' @param limit Optional maximum number of search results per query.
#' @param overwrite Logical; passed to `hdar` as `force`.
#' @param prompt Logical; allow `hdar` to request confirmation before download.
#' @param quiet Logical; suppress TSGenerator progress messages.
#' @param retries Number of retries after transient WEkEO search/download failures.
#' @param backoff Initial retry delay in seconds; exponential backoff is used.
#'
#' @return An object of class `tsg_vpp_download` containing the submitted
#'   queries, WEkEO search results, request summary, output directory and
#'   download status.
#' @family WEkEO acquisition
#' @export
#'
#' @examples
#' \dontrun{
#' # Preview SOS and EOS dates for the first season
#' x <- download_vpp(
#'   start = "2020-01-01", end = "2020-12-31",
#'   tile_id = "30TXM", product = c("SOSD", "EOSD"),
#'   season = "s1", download = FALSE
#' )
#'
#' # Download core phenology metrics for both seasons
#' x <- download_vpp(
#'   start = "2020-01-01", end = "2021-12-31",
#'   tile_id = "30TXM",
#'   product = c("SOSD", "MAXD", "EOSD", "LENGTH"),
#'   season = c("s1", "s2"), output_dir = "HRVPP_VPP"
#' )
#' }
download_vpp <- function(start, end,
                         output_dir = "HRVPP_VPP",
                         product = c("SOSD", "MAXD", "EOSD", "LENGTH"),
                         season = "s1",
                         tile_id = NULL,
                         bbox = NULL,
                         platform = "S2A, S2B",
                         product_version = NULL,
                         client = NULL,
                         download = TRUE,
                         limit = NULL,
                         overwrite = FALSE,
                         prompt = interactive(),
                         quiet = FALSE,
                         retries = 2L,
                         backoff = 2) {
  .vpp_require_backend()
  dates <- .vpp_validate_dates(start, end)
  product <- .vpp_validate_product(product)
  season <- .vpp_validate_season(season)
  tile_id <- .vpp_validate_tile(tile_id)
  bbox <- .vpp_validate_bbox(bbox)
  platform <- .vpp_validate_platform(platform)

  if (!is.null(product_version) &&
      (!is.character(product_version) || length(product_version) != 1L || !nzchar(product_version))) {
    stop("'product_version' must be NULL or one non-empty character string.", call. = FALSE)
  }
  .vpp_validate_flag(download, "download")
  .vpp_validate_flag(overwrite, "overwrite")
  .vpp_validate_flag(prompt, "prompt")
  if (!is.null(limit) && (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit < 1)) {
    stop("'limit' must be NULL or a positive number.", call. = FALSE)
  }

  if (is.null(client)) client <- hda_client()
  temporal_fields <- .hda_temporal_fields(.VPP_DATASET_ID, client = client)
  groups <- if (is.null(season)) NA_character_ else season
  requests <- vector("list", length(product) * length(groups))
  k <- 0L

  if (isTRUE(download)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) stop("Could not create output directory: ", output_dir, call. = FALSE)
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  }

  for (p in product) {
    for (g in groups) {
      k <- k + 1L
      group_value <- if (is.na(g)) NULL else g
      q <- .vpp_build_query(
        product = p, season = group_value,
        start = dates$start, end = dates$end,
        tile_id = tile_id, bbox = bbox, platform = platform,
        product_version = product_version,
        temporal_fields = temporal_fields
      )

      label <- if (is.null(group_value)) "all seasons" else group_value
      if (!isTRUE(quiet)) {
        message("Searching VPP ", p, " (", label, "): ", dates$start, " to ", dates$end, "...")
      }
      sr <- tryCatch(
        hda_search(q$json, client = client, limit = limit, retries = retries, backoff = backoff),
        error = function(e) stop(
          "WEkEO VPP search failed for ", p, " (", label, "): ",
          conditionMessage(e), call. = FALSE
        )
      )

      meta <- .vpp_result_metadata(sr)
      n_found <- .vpp_result_count(sr)
      skipped <- character()
      selected_indexes <- NULL
      n_to_download <- n_found
      if (isTRUE(download) && !isTRUE(overwrite) && nrow(meta) > 0L) {
        keep <- !vapply(meta$id, .vpp_product_exists, logical(1), output_dir = output_dir)
        skipped <- meta$id[!keep]
        selected_indexes <- which(keep)
        n_to_download <- length(selected_indexes)
      }

      status <- if (!isTRUE(download)) {
        "preview"
      } else if (n_to_download == 0L) {
        "nothing_to_download"
      } else {
        "downloaded"
      }

      if (isTRUE(download) && n_to_download > 0L) {
        if (!isTRUE(quiet)) message("Downloading ", n_to_download, " VPP item(s)...")
        .tsg_retry(function() {
          if (is.null(selected_indexes)) {
            sr$download(output_dir, force = isTRUE(overwrite), prompt = isTRUE(prompt))
          } else {
            sr$download(output_dir, selected_indexes = selected_indexes,
                        force = isTRUE(overwrite), prompt = isTRUE(prompt))
          }
        }, retries = retries, backoff = backoff, label = "WEkEO VPP download")
      }

      requests[[k]] <- list(
        product = p,
        season = group_value,
        start = dates$start,
        end = dates$end,
        query = q$list,
        query_json = q$json,
        results = sr,
        metadata = meta,
        n_results = n_found,
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
      season = if (is.null(z$season)) "all" else z$season,
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
    dataset_id = .VPP_DATASET_ID,
    products = product,
    seasons = season,
    requested_start = dates$start,
    requested_end = dates$end,
    tile_id = tile_id,
    bbox = bbox,
    output_dir = if (isTRUE(download)) output_dir else NULL,
    downloaded = isTRUE(download),
    requests = requests,
    summary = summary
  )
  class(out) <- "tsg_vpp_download"
  out
}

#' @family WEkEO acquisition
#' @export
print.tsg_vpp_download <- function(x, ...) {
  cat("TSGenerator HR-VPP Vegetation Phenology and Productivity request\n")
  cat("Dataset:  ", x$dataset_id, "\n", sep = "")
  cat("Period:   ", as.character(x$requested_start), " to ", as.character(x$requested_end), "\n", sep = "")
  cat("Products: ", paste(x$products, collapse = ", "), "\n", sep = "")
  cat("Seasons:  ", if (is.null(x$seasons)) "all/unfiltered" else paste(x$seasons, collapse = ", "), "\n", sep = "")
  if (!is.null(x$tile_id)) cat("Tile:      ", x$tile_id, "\n", sep = "")
  cat("Mode:      ", if (isTRUE(x$downloaded)) "download" else "preview", "\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

.vpp_require_backend <- function() {
  if (!requireNamespace("hdar", quietly = TRUE)) {
    stop("Package 'hdar' is required. Install it with install.packages('hdar').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').", call. = FALSE)
  }
  invisible(TRUE)
}

.vpp_validate_dates <- function(start, end) {
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

.vpp_validate_product <- function(product) {
  if (!is.character(product) || length(product) < 1L || anyNA(product)) {
    stop("'product' must contain one or more supported VPP product types.", call. = FALSE)
  }
  product <- unique(toupper(trimws(product)))
  if (length(product) == 1L && product == "ALL") return(.VPP_PRODUCTS)
  if ("ALL" %in% product) stop("Use product = 'all' alone, or list individual VPP products.", call. = FALSE)
  bad <- setdiff(product, .VPP_PRODUCTS)
  if (length(bad)) {
    stop("Unsupported VPP product(s): ", paste(bad, collapse = ", "),
         ". Supported products: ", paste(.VPP_PRODUCTS, collapse = ", "), ".", call. = FALSE)
  }
  product
}

.vpp_validate_season <- function(season) {
  if (is.null(season)) return(NULL)
  if (!is.character(season) || length(season) < 1L || anyNA(season)) {
    stop("'season' must be NULL, 's1', 's2', or both.", call. = FALSE)
  }
  season <- unique(tolower(trimws(season)))
  bad <- setdiff(season, .VPP_SEASONS)
  if (length(bad)) stop("Unsupported VPP season(s): ", paste(bad, collapse = ", "), ".", call. = FALSE)
  season
}

.vpp_validate_tile <- function(tile_id) {
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

.vpp_validate_bbox <- function(bbox) {
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

.vpp_validate_platform <- function(platform) {
  if (is.null(platform)) return(NULL)
  if (!is.character(platform) || length(platform) != 1L || platform != "S2A, S2B") {
    stop("'platform' must be NULL or 'S2A, S2B' for the current VPP dataset.", call. = FALSE)
  }
  platform
}

.vpp_validate_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

.vpp_iso_start <- function(x) paste0(as.character(x), "T00:00:00.000Z")
.vpp_iso_end <- function(x) paste0(as.character(x), "T23:59:59.999Z")

.vpp_build_query <- function(product, season, start, end, tile_id, bbox,
                             platform, product_version,
                             temporal_fields = c(start = "start", end = "end")) {
  q <- list(
    dataset_id = .VPP_DATASET_ID,
    productType = product,
    itemsPerPage = 200L,
    startIndex = 0L
  )
  q[[unname(temporal_fields[["start"]])]] <- .vpp_iso_start(start)
  q[[unname(temporal_fields[["end"]])]] <- .vpp_iso_end(end)
  if (!is.null(platform)) q$platformSerialIdentifier <- platform
  if (!is.null(tile_id)) q$tileId <- tile_id
  if (!is.null(product_version)) q$productVersion <- product_version
  if (!is.null(season)) q$productGroupId <- season
  if (!is.null(bbox)) q$bbox <- bbox
  json <- jsonlite::toJSON(q, auto_unbox = TRUE, digits = NA)
  list(list = q, json = json)
}

.vpp_result_count <- function(results) {
  if (is.null(results)) return(0L)
  n <- tryCatch(length(results$results), error = function(e) NA_integer_)
  if (is.na(n)) n <- tryCatch(length(results), error = function(e) NA_integer_)
  if (is.na(n)) 0L else as.integer(n)
}

.vpp_result_metadata <- function(results) {
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

.vpp_product_exists <- function(id, output_dir) {
  if (is.na(id) || !nzchar(id)) return(FALSE)
  files <- list.files(output_dir, recursive = TRUE, full.names = FALSE)
  any(startsWith(basename(files), id))
}
