# Acquisition robustness and diagnostics ---------------------------------

#' Check the TSGenerator WEkEO acquisition environment
#'
#' Performs non-destructive checks of the native R acquisition stack. By
#' default no network request is made. With `online = TRUE`, authentication
#' and the ST/VPP dataset query schemas are checked against the live service.
#'
#' @param client Optional client created by [hda_client()].
#' @param online Logical; perform live WEkEO checks.
#' @return A data frame of diagnostic checks with class `tsg_wekeo_check`.
#' @family WEkEO acquisition
#' @export
check_wekeo <- function(client = NULL, online = FALSE) {
  if (!is.logical(online) || length(online) != 1L || is.na(online)) {
    stop("'online' must be TRUE or FALSE.", call. = FALSE)
  }
  rows <- list()
  add <- function(check, ok, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(check = check, ok = isTRUE(ok), detail = as.character(detail), stringsAsFactors = FALSE)
  }
  add("R", TRUE, paste(R.version$major, R.version$minor, sep = "."))
  add("hdar", requireNamespace("hdar", quietly = TRUE), if (requireNamespace("hdar", quietly = TRUE)) as.character(utils::packageVersion("hdar")) else "not installed")
  add("jsonlite", requireNamespace("jsonlite", quietly = TRUE), if (requireNamespace("jsonlite", quietly = TRUE)) as.character(utils::packageVersion("jsonlite")) else "not installed")
  hdarc <- path.expand("~/.hdarc")
  add("credentials_file", file.exists(hdarc), if (file.exists(hdarc)) hdarc else "~/.hdarc not found (credentials may still be supplied explicitly)")

  if (isTRUE(online)) {
    if (!requireNamespace("hdar", quietly = TRUE)) stop("Package 'hdar' is required for online diagnostics.", call. = FALSE)
    if (is.null(client)) client <- tryCatch(hda_client(), error = function(e) e)
    if (inherits(client, "error")) {
      add("authentication", FALSE, conditionMessage(client))
    } else {
      auth <- tryCatch({ hda_auth_check(client); TRUE }, error = function(e) e)
      add("authentication", isTRUE(auth), if (isTRUE(auth)) "OK" else conditionMessage(auth))
      datasets <- c(ST = "EO:EEA:DAT:CLMS_HRVPP_ST", VPP = "EO:EEA:DAT:CLMS_HRVPP_VPP")
      for (nm in names(datasets)) {
        ds <- unname(datasets[[nm]])
        z <- tryCatch(hda_query_template(ds, client = client), error = function(e) e)
        add(paste0("schema_", nm), !inherits(z, "error"), if (inherits(z, "error")) conditionMessage(z) else "queryable schema available")
      }
    }
  }
  out <- do.call(rbind, rows)
  class(out) <- c("tsg_wekeo_check", class(out))
  out
}

#' @family WEkEO acquisition
#' @export
print.tsg_wekeo_check <- function(x, ...) {
  cat("TSGenerator WEkEO acquisition diagnostics\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

.tsg_retry <- function(fun, retries = 2L, backoff = 2, label = "WEkEO operation") {
  retries <- as.integer(retries)
  if (is.na(retries) || retries < 0L) stop("'retries' must be zero or a positive integer.", call. = FALSE)
  if (!is.numeric(backoff) || length(backoff) != 1L || is.na(backoff) || backoff < 0) stop("'backoff' must be a non-negative number.", call. = FALSE)
  last <- NULL
  for (attempt in seq_len(retries + 1L)) {
    ans <- tryCatch(list(ok = TRUE, value = fun()), error = function(e) list(ok = FALSE, error = e))
    if (isTRUE(ans$ok)) return(ans$value)
    last <- ans$error
    if (attempt <= retries) Sys.sleep(backoff * (2 ^ (attempt - 1L)))
  }
  stop(label, " failed after ", retries + 1L, " attempt(s): ", conditionMessage(last), call. = FALSE)
}

.tsg_bytes <- function(metadata, indexes = NULL) {
  if (is.null(metadata) || !nrow(metadata) || !"size" %in% names(metadata)) return(0)
  x <- metadata$size
  if (!is.null(indexes)) x <- x[indexes]
  sum(x[is.finite(x)], na.rm = TRUE)
}

.tsg_human_size <- function(bytes) {
  if (!is.finite(bytes) || bytes <= 0) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- min(floor(log(bytes, 1024)), length(units) - 1L)
  sprintf("%.2f %s", bytes / (1024 ^ i), units[i + 1L])
}
