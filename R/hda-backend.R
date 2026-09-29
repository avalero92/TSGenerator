# Native WEkEO HDA backend -------------------------------------------------

#' Create a native WEkEO HDA client
#'
#' Creates an authenticated client using the R package `hdar`. If `username`
#' and `password` are omitted, `hdar` reads credentials from `~/.hdarc`.
#' Credentials are never written by TSGenerator unless `save_credentials = TRUE`.
#'
#' @param username Optional WEkEO username.
#' @param password Optional WEkEO password.
#' @param save_credentials Logical; allow `hdar` to save credentials in `~/.hdarc`.
#' @return An authenticated `hdar::Client` object.
#' @family WEkEO acquisition
#' @export
hda_client <- function(username = NULL, password = NULL,
                       save_credentials = FALSE) {
  if (!requireNamespace("hdar", quietly = TRUE)) {
    stop("Package 'hdar' is required for native WEkEO access. Install it with install.packages('hdar').",
         call. = FALSE)
  }
  if (xor(is.null(username), is.null(password))) {
    stop("Provide both 'username' and 'password', or neither to use ~/.hdarc.", call. = FALSE)
  }
  if (is.null(username)) {
    client <- hdar::Client$new()
  } else {
    client <- hdar::Client$new(username, password,
                               save_credentials = isTRUE(save_credentials))
  }
  client
}

#' Test native WEkEO authentication
#'
#' @param client Optional client created by [hda_client()].
#' @param ... Passed to [hda_client()] when `client` is omitted.
#' @return Invisibly returns `TRUE` after a token is obtained.
#' @family WEkEO acquisition
#' @export
hda_auth_check <- function(client = NULL, ...) {
  if (is.null(client)) client <- hda_client(...)
  token <- tryCatch(client$get_token(), error = function(e) {
    stop("WEkEO authentication failed: ", conditionMessage(e), call. = FALSE)
  })
  if (is.null(token) || length(token) == 0L) {
    stop("WEkEO authentication did not return a token.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Discover WEkEO datasets
#'
#' @param pattern Optional text used to filter the WEkEO catalogue.
#' @param client Optional authenticated HDA client.
#' @return The dataset records returned by `hdar`.
#' @family WEkEO acquisition
#' @export
hda_datasets <- function(pattern = NULL, client = NULL) {
  if (is.null(client)) client <- hda_client()
  if (is.null(pattern) || !nzchar(pattern)) client$datasets() else client$datasets(pattern)
}

#' Generate an HDA V2 query template
#'
#' Queries are generated from the live WEkEO `/queryable` metadata rather than
#' hard-coding provider-specific parameter names.
#'
#' @param dataset_id WEkEO dataset identifier.
#' @param client Optional authenticated HDA client.
#' @return A query template returned by `hdar`.
#' @family WEkEO acquisition
#' @export
hda_query_template <- function(dataset_id, client = NULL) {
  .validate_dataset_id(dataset_id)
  if (is.null(client)) client <- hda_client()
  client$generate_query_template(dataset_id)
}

#' Search WEkEO using an HDA V2 query
#'
#' @param query HDA V2 query, normally based on [hda_query_template()].
#' @param client Optional authenticated HDA client.
#' @param limit Optional maximum number of results supported by `hdar`.
#' @param retries Number of retries after transient failures.
#' @param backoff Initial retry delay in seconds.
#' @return An `hdar` SearchResults object.
#' @family WEkEO acquisition
#' @export
hda_search <- function(query, client = NULL, limit = NULL, retries = 2L, backoff = 2) {
  if (missing(query) || is.null(query)) stop("'query' is required.", call. = FALSE)
  if (is.null(client)) client <- hda_client()
  .tsg_retry(function() {
    if (is.null(limit)) client$search(query) else client$search(query, limit = limit)
  }, retries = retries, backoff = backoff, label = 'WEkEO search')
}

#' Download HDA search results
#'
#' @param results SearchResults returned by [hda_search()].
#' @param output_dir Destination directory. It is created when necessary.
#' @param force Logical; re-download existing files when supported by `hdar`.
#' @param retries Number of retries after transient failures.
#' @param backoff Initial retry delay in seconds.
#' @param prompt Logical; allow `hdar` to request confirmation.
#' @return The value returned by `hdar`'s download method, invisibly.
#' @family WEkEO acquisition
#' @export
hda_download <- function(results, output_dir, force = FALSE, retries = 2L, backoff = 2, prompt = interactive()) {
  if (missing(results) || is.null(results)) stop("'results' is required.", call. = FALSE)
  if (missing(output_dir) || !is.character(output_dir) || length(output_dir) != 1L || !nzchar(output_dir)) {
    stop("'output_dir' must be one non-empty path.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output directory: ", output_dir, call. = FALSE)
  out <- .tsg_retry(function() results$download(output_dir, force = isTRUE(force), prompt = isTRUE(prompt)),
                    retries = retries, backoff = backoff, label = 'WEkEO download')
  invisible(out)
}

.validate_dataset_id <- function(dataset_id) {
  if (!is.character(dataset_id) || length(dataset_id) != 1L || !nzchar(dataset_id)) {
    stop("'dataset_id' must be one non-empty character string.", call. = FALSE)
  }
  invisible(TRUE)
}


# Resolve live temporal query fields --------------------------------------

.hda_template_list <- function(template) {
  if (is.list(template)) return(template)
  if (is.character(template) && length(template) == 1L && nzchar(template)) {
    return(jsonlite::fromJSON(template, simplifyVector = FALSE))
  }
  stop("WEkEO query template has an unsupported format.", call. = FALSE)
}

.hda_temporal_fields <- function(dataset_id, client = NULL) {
  template <- .hda_template_list(hda_query_template(dataset_id, client = client))
  nms <- names(template)
  if (all(c("start", "end") %in% nms)) return(c(start = "start", end = "end"))
  if (all(c("startdate", "enddate") %in% nms)) return(c(start = "startdate", end = "enddate"))
  if (all(c("dtstart", "dtend") %in% nms)) return(c(start = "dtstart", end = "dtend"))
  stop(
    "Could not identify the live WEkEO temporal fields for dataset '", dataset_id,
    "'. Available fields: ", paste(nms, collapse = ", "), ".",
    call. = FALSE
  )
}
