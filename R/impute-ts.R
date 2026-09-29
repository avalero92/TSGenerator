#' Impute missing values in vegetation time series
#'
#' `impute_ts()` provides explicit, opt-in imputation for user-supplied or raw
#' vegetation time series. It never imputes silently and preserves both the
#' original values and an indicator identifying values created by imputation.
#'
#' Seasonal Trajectories (ST) are already temporally processed Copernicus
#' products. For this reason, imputation of ST is blocked by default and must
#' be explicitly enabled with `allow_processed = TRUE`.
#'
#' @param data A data frame or a `tsg_temporal_plan`.
#' @param id_col Identifier column. Ignored when `data` is a temporal plan.
#' @param date_col Date column. Ignored when `data` is a temporal plan.
#' @param value_col Value column. Ignored when `data` is a temporal plan.
#' @param method Imputation method: `"linear"` or `"kalman"`.
#' @param series_type One of `"user"`, `"raw"`, or `"st"`.
#' @param allow_processed Logical. Required to impute `series_type = "st"`.
#' @param complete_grid Logical. If `TRUE`, explicitly inserts missing expected
#'   dates before imputation.
#' @param expected_step Expected temporal step in days. Required when
#'   `complete_grid = TRUE` unless available in a temporal plan.
#' @param max_gap Maximum number of consecutive missing observations eligible
#'   for imputation. `NULL` imposes no gap-length restriction.
#' @param allow_edge Logical. If `FALSE`, leading and trailing missing runs are
#'   retained as `NA` even if the selected method can estimate them.
#' @param output_col Name of the completed-value column.
#' @return A data frame of class `tsg_imputed_series`. The original value column
#'   is preserved and additional columns identify imputed values and inserted
#'   expected dates.
#' @family time-series analysis
#' @export
impute_ts <- function(data, id_col = "ID", date_col = "Date", value_col = "Value",
                      method = c("linear", "kalman"),
                      series_type = c("user", "raw", "st"),
                      allow_processed = FALSE, complete_grid = FALSE,
                      expected_step = NULL, max_gap = NULL,
                      allow_edge = FALSE, output_col = NULL) {
  method <- match.arg(method)
  series_type <- match.arg(series_type)

  if (inherits(data, "tsg_temporal_plan")) {
    plan <- data
    id_col <- plan$id_col
    date_col <- plan$date_col
    value_col <- plan$value_col
    if (is.null(expected_step)) expected_step <- plan$expected_step
    data <- plan$data
  } else {
    plan <- temporal_plan(data, id_col, date_col, value_col,
                          expected_step = expected_step,
                          allow_duplicates = FALSE)
    data <- plan$data
  }

  if (identical(series_type, "st") && !isTRUE(allow_processed)) {
    stop("ST Seasonal Trajectories are already temporally processed. Imputation is blocked by default. Set `allow_processed = TRUE` only when this additional processing is scientifically justified.", call. = FALSE)
  }
  if (!is.numeric(data[[value_col]])) {
    stop("`value_col` must be numeric for imputation.", call. = FALSE)
  }
  if (isTRUE(complete_grid)) {
    if (is.null(expected_step) || length(expected_step) != 1L || !is.numeric(expected_step) ||
        is.na(expected_step) || expected_step <= 0) {
      stop("A positive `expected_step` is required when `complete_grid = TRUE`.", call. = FALSE)
    }
  }
  if (!is.null(max_gap)) {
    if (length(max_gap) != 1L || !is.numeric(max_gap) || is.na(max_gap) || max_gap < 1 || max_gap != floor(max_gap)) {
      stop("`max_gap` must be NULL or one positive integer number of observations.", call. = FALSE)
    }
  }
  if (is.null(output_col)) output_col <- paste0(value_col, "_imputed")
  if (!is.character(output_col) || length(output_col) != 1L || !nzchar(output_col)) {
    stop("`output_col` must be one non-empty column name.", call. = FALSE)
  }
  if (output_col %in% names(data) && output_col != value_col) {
    stop("`output_col` already exists in `data`.", call. = FALSE)
  }

  ids <- unique(data[[id_col]])
  pieces <- lapply(ids, function(id) {
    z <- data[data[[id_col]] == id, , drop = FALSE]
    z <- z[order(z[[date_col]]), , drop = FALSE]
    z$.InsertedDate <- FALSE

    if (isTRUE(complete_grid) && nrow(z)) {
      full_dates <- seq(min(z[[date_col]], na.rm = TRUE), max(z[[date_col]], na.rm = TRUE), by = expected_step)
      grid <- data.frame(.date = full_dates)
      names(grid) <- date_col
      z$.original_order <- seq_len(nrow(z))
      z <- merge(grid, z, by = date_col, all.x = TRUE, sort = TRUE)
      z[[id_col]][is.na(z[[id_col]])] <- id
      z$.InsertedDate <- is.na(z$.original_order)
      z$.original_order <- NULL
    }

    original <- z[[value_col]]
    completed <- .tsg_impute_vector(original, method = method)

    # By default, do not fill leading/trailing missing runs.
    if (!isTRUE(allow_edge) && any(!is.na(original))) {
      observed <- which(!is.na(original))
      completed[seq_len(min(observed) - 1L)] <- NA_real_
      if (max(observed) < length(original)) completed[(max(observed) + 1L):length(original)] <- NA_real_
    }

    # Optionally retain missing runs that are longer than the accepted limit.
    if (!is.null(max_gap)) {
      runs <- rle(is.na(original))
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1L
      too_long <- which(runs$values & runs$lengths > max_gap)
      for (j in too_long) completed[starts[j]:ends[j]] <- NA_real_
    }

    z[[output_col]] <- completed
    z$.WasImputed <- is.na(original) & !is.na(completed)
    z
  })

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  attr(out, "imputation_method") <- method
  attr(out, "series_type") <- series_type
  attr(out, "expected_step") <- expected_step
  class(out) <- c("tsg_imputed_series", "data.frame")
  out
}

.tsg_impute_vector <- function(x, method) {
  if (!anyNA(x) || all(is.na(x))) return(as.numeric(x))
  if (identical(method, "linear")) {
    ok <- which(!is.na(x))
    if (length(ok) < 2L) return(as.numeric(x))
    return(stats::approx(x = ok, y = x[ok], xout = seq_along(x), method = "linear", rule = 1)$y)
  }
  if (!requireNamespace("imputeTS", quietly = TRUE)) {
    stop("Method 'kalman' requires the `imputeTS` package.", call. = FALSE)
  }
  as.numeric(imputeTS::na_kalman(x, model = "StructTS", smooth = TRUE))
}

#' @family time-series analysis
#' @export
print.tsg_imputed_series <- function(x, ...) {
  method <- attr(x, "imputation_method")
  type <- attr(x, "series_type")
  cat("TSGenerator imputed time series\n")
  cat("Method:      ", method, "\n", sep = "")
  cat("Series type: ", type, "\n", sep = "")
  cat("Rows:        ", nrow(x), "\n", sep = "")
  if (".WasImputed" %in% names(x)) cat("Imputed:     ", sum(x$.WasImputed, na.rm = TRUE), " values\n", sep = "")
  invisible(x)
}
