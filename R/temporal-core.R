#' Inspect and validate a vegetation time-series before analysis
#'
#' `temporal_plan()` is the entry point to the TSGenerator 2.0 temporal-analysis
#' core. It validates identifiers, dates, values, duplicate observations and
#' temporal regularity without modifying or imputing the input data.
#'
#' @param data A data frame containing a time series.
#' @param id_col Name of the series identifier column.
#' @param date_col Name of the date column.
#' @param value_col Name of the value column to assess.
#' @param expected_step Optional expected temporal step in days. If `NULL`, the
#'   modal positive interval observed in the data is reported but not enforced.
#' @param allow_duplicates Logical. If `FALSE`, duplicate ID-date observations
#'   cause an error.
#' @return An object of class `tsg_temporal_plan` containing validated data and
#'   diagnostics by series.
#' @family time-series analysis
#' @export
temporal_plan <- function(data, id_col = "ID", date_col = "Date",
                          value_col = "Value", expected_step = NULL,
                          allow_duplicates = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  cols <- c(id_col, date_col, value_col)
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols)) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (anyNA(data[[id_col]])) stop("`id_col` contains missing values.", call. = FALSE)
  dates <- as.Date(data[[date_col]])
  if (anyNA(dates) && any(!is.na(data[[date_col]]))) {
    stop("`date_col` contains values that cannot be converted to Date.", call. = FALSE)
  }
  if (!is.null(expected_step)) {
    if (length(expected_step) != 1L || !is.numeric(expected_step) || is.na(expected_step) || expected_step <= 0) {
      stop("`expected_step` must be one positive number of days or NULL.", call. = FALSE)
    }
  }

  work <- data
  work[[date_col]] <- dates
  key <- paste(work[[id_col]], work[[date_col]], sep = "\r")
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(dup) && !isTRUE(allow_duplicates)) {
    stop("Duplicate ID-date observations detected. Resolve duplicates or set `allow_duplicates = TRUE`.", call. = FALSE)
  }

  ids <- unique(work[[id_col]])
  diagnostics <- lapply(ids, function(id) {
    idx <- work[[id_col]] == id
    d <- sort(unique(work[[date_col]][idx & !is.na(work[[date_col]])]))
    gaps <- as.numeric(diff(d))
    positive <- gaps[gaps > 0]
    modal_step <- if (length(positive)) {
      u <- sort(unique(positive)); u[which.max(tabulate(match(positive, u)))]
    } else NA_real_
    step <- if (is.null(expected_step)) modal_step else expected_step
    irregular <- if (length(positive) && !is.na(step)) sum(positive != step) else 0L
    vals <- work[[value_col]][idx]
    data.frame(
      ID = as.character(id),
      n_rows = sum(idx),
      n_dates = length(d),
      start = if (length(d)) min(d) else as.Date(NA),
      end = if (length(d)) max(d) else as.Date(NA),
      n_missing = sum(is.na(vals)),
      missing_fraction = if (length(vals)) mean(is.na(vals)) else NA_real_,
      modal_step_days = modal_step,
      irregular_intervals = irregular,
      duplicate_rows = sum(dup & idx),
      stringsAsFactors = FALSE
    )
  })
  diagnostics <- do.call(rbind, diagnostics)
  names(diagnostics)[1] <- id_col

  out <- list(
    data = work,
    diagnostics = diagnostics,
    id_col = id_col,
    date_col = date_col,
    value_col = value_col,
    expected_step = expected_step,
    has_duplicates = any(dup)
  )
  class(out) <- "tsg_temporal_plan"
  out
}

#' @family time-series analysis
#' @export
print.tsg_temporal_plan <- function(x, ...) {
  cat("TSGenerator temporal analysis plan\n")
  cat("Series:   ", nrow(x$diagnostics), "\n", sep = "")
  cat("ID:       ", x$id_col, "\n", sep = "")
  cat("Date:     ", x$date_col, "\n", sep = "")
  cat("Value:    ", x$value_col, "\n", sep = "")
  if (!is.null(x$expected_step)) cat("Expected: ", x$expected_step, " days\n", sep = "")
  cat("Missing:  ", sum(x$diagnostics$n_missing), " values\n", sep = "")
  cat("Duplicates: ", if (isTRUE(x$has_duplicates)) "yes" else "no", "\n", sep = "")
  invisible(x)
}
