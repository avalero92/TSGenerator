#' Summarise missingness and temporal completeness
#'
#' Quantifies two different forms of missing information in a time series:
#' explicit `NA` values on observed dates and expected dates that are absent
#' from the table. Expected dates are derived from `expected_step`.
#'
#' @param data A data frame, or a `tsg_temporal_plan` object.
#' @param id_col,date_col,value_col Column names used when `data` is a data frame.
#' @param expected_step Expected temporal interval in days. Required unless it is
#'   available in a `tsg_temporal_plan` object.
#' @param start,end Optional common analysis limits coercible to `Date`. If omitted,
#'   each series is assessed between its first and last observed date.
#' @return A data frame with one row per series and counts/fractions describing
#'   expected dates, observed dates, absent dates, explicit NA values and usable values.
#' @family time-series analysis
#' @export
summarize_missingness <- function(data, id_col = "ID", date_col = "Date",
                                  value_col = "Value", expected_step = NULL,
                                  start = NULL, end = NULL) {
  z <- .tsg_temporal_input(data, id_col, date_col, value_col, expected_step)
  d <- z$data; id_col <- z$id_col; date_col <- z$date_col; value_col <- z$value_col
  step <- z$expected_step
  if (is.null(step)) stop("`expected_step` is required to quantify absent expected dates.", call. = FALSE)
  .tsg_positive_step(step)
  if (!is.null(start)) start <- .tsg_one_date(start, "start")
  if (!is.null(end)) end <- .tsg_one_date(end, "end")
  if (!is.null(start) && !is.null(end) && start > end) stop("`start` must not be after `end`.", call. = FALSE)

  key <- paste(d[[id_col]], d[[date_col]], sep = "\r")
  if (anyDuplicated(key)) stop("Duplicate ID-date observations are not supported by `summarize_missingness()`.", call. = FALSE)
  ids <- unique(d[[id_col]])
  ans <- lapply(ids, function(id) {
    q <- d[d[[id_col]] == id & !is.na(d[[date_col]]), , drop = FALSE]
    obs_dates <- sort(unique(q[[date_col]]))
    s <- if (!is.null(start)) start else if (length(obs_dates)) min(obs_dates) else as.Date(NA)
    e <- if (!is.null(end)) end else if (length(obs_dates)) max(obs_dates) else as.Date(NA)
    expected <- if (!is.na(s) && !is.na(e) && s <= e) seq.Date(s, e, by = paste(step, "days")) else as.Date(character())
    in_window <- if (length(expected)) q[[date_col]] >= s & q[[date_col]] <= e else rep(FALSE, nrow(q))
    qw <- q[in_window, , drop = FALSE]
    present <- unique(qw[[date_col]])
    n_expected <- length(expected)
    n_observed <- sum(expected %in% present)
    n_absent <- n_expected - n_observed
    # NA is counted only where an expected date is actually represented by a row.
    n_value_na <- if (nrow(qw)) sum(is.na(qw[[value_col]]) & qw[[date_col]] %in% expected) else 0L
    n_usable <- max(0L, n_observed - n_value_na)
    data.frame(
      ID = as.character(id), start = s, end = e,
      expected_step_days = as.numeric(step), n_expected = n_expected,
      n_observed_dates = n_observed, n_absent_dates = n_absent,
      n_value_na = n_value_na, n_usable = n_usable,
      date_completeness = if (n_expected) n_observed / n_expected else NA_real_,
      value_completeness = if (n_expected) n_usable / n_expected else NA_real_,
      missing_fraction = if (n_expected) (n_absent + n_value_na) / n_expected else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, ans); names(out)[1] <- id_col; rownames(out) <- NULL
  class(out) <- c("tsg_missingness", class(out)); out
}

#' Assess time-series completeness quality
#'
#' Classifies temporal completeness from the fraction of expected observations
#' containing usable (non-missing) values. This is distinct from Copernicus
#' ST/VPP product QFLAG quality handled by `quality_info()` and related functions.
#'
#' @param data A data frame or `tsg_temporal_plan` object.
#' @param id_col,date_col,value_col Column names.
#' @param expected_step Expected interval in days.
#' @param window_days Optional positive window length in days. `NULL` assesses the
#'   complete series; e.g. `91` produces consecutive 91-day assessment windows.
#' @param thresholds Named numeric vector with `high`, `medium`, and `low` lower
#'   completeness bounds. Defaults to 0.90, 0.75 and 0.50.
#' @param start,end Optional common assessment limits.
#' @return An object of class `tsg_ts_quality` with one row per series/window.
#' @family time-series analysis
#' @export
assess_ts_quality <- function(data, id_col = "ID", date_col = "Date",
                              value_col = "Value", expected_step = NULL,
                              window_days = NULL,
                              thresholds = c(high = 0.90, medium = 0.75, low = 0.50),
                              start = NULL, end = NULL) {
  th <- .tsg_quality_thresholds(thresholds)
  z <- .tsg_temporal_input(data, id_col, date_col, value_col, expected_step)
  if (is.null(z$expected_step)) stop("`expected_step` is required for completeness assessment.", call. = FALSE)
  if (is.null(window_days)) {
    out <- summarize_missingness(z$data, z$id_col, z$date_col, z$value_col,
                                 z$expected_step, start, end)
    out$window <- 1L
  } else {
    if (length(window_days) != 1L || !is.numeric(window_days) || is.na(window_days) || window_days <= 0) {
      stop("`window_days` must be one positive number of days or NULL.", call. = FALSE)
    }
    d <- z$data
    global_start <- if (is.null(start)) min(d[[z$date_col]], na.rm = TRUE) else .tsg_one_date(start, "start")
    global_end <- if (is.null(end)) max(d[[z$date_col]], na.rm = TRUE) else .tsg_one_date(end, "end")
    if (!is.finite(as.numeric(global_start)) || !is.finite(as.numeric(global_end))) stop("No valid dates available.", call. = FALSE)
    starts <- seq.Date(global_start, global_end, by = paste(as.integer(window_days), "days"))
    pieces <- lapply(seq_along(starts), function(i) {
      ws <- starts[i]; we <- min(ws + as.integer(window_days) - 1L, global_end)
      x <- summarize_missingness(d, z$id_col, z$date_col, z$value_col, z$expected_step, ws, we)
      x$window <- i; x
    })
    out <- do.call(rbind, pieces); rownames(out) <- NULL
  }
  cpl <- out$value_completeness
  out$Quality <- ifelse(is.na(cpl), NA_character_,
                        ifelse(cpl >= th["high"], "High",
                               ifelse(cpl >= th["medium"], "Medium",
                                      ifelse(cpl >= th["low"], "Low", "Very low"))))
  out$Quality <- factor(out$Quality, levels = c("Very low", "Low", "Medium", "High"), ordered = TRUE)
  attr(out, "thresholds") <- th
  attr(out, "window_days") <- window_days
  class(out) <- c("tsg_ts_quality", setdiff(class(out), "tsg_missingness"))
  out
}

#' @family time-series analysis
#' @export
print.tsg_ts_quality <- function(x, ...) {
  cat("TSGenerator time-series quality assessment\n")
  cat("Rows:     ", nrow(x), "\n", sep = "")
  wd <- attr(x, "window_days")
  cat("Windows:  ", if (is.null(wd)) "full series" else paste0(wd, " days"), "\n", sep = "")
  q <- table(x$Quality, useNA = "ifany")
  if (length(q)) print(q)
  invisible(x)
}

.tsg_temporal_input <- function(data, id_col, date_col, value_col, expected_step) {
  if (inherits(data, "tsg_temporal_plan")) {
    if (is.null(expected_step)) expected_step <- data$expected_step
    return(list(data = data$data, id_col = data$id_col, date_col = data$date_col,
                value_col = data$value_col, expected_step = expected_step))
  }
  p <- temporal_plan(data, id_col, date_col, value_col,
                     expected_step = expected_step, allow_duplicates = FALSE)
  list(data = p$data, id_col = p$id_col, date_col = p$date_col,
       value_col = p$value_col, expected_step = expected_step)
}

.tsg_positive_step <- function(x) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || x <= 0) stop("`expected_step` must be one positive number of days.", call. = FALSE)
  invisible(TRUE)
}

.tsg_one_date <- function(x, name) {
  d <- as.Date(x)
  if (length(d) != 1L || is.na(d)) stop("`", name, "` must be one valid date.", call. = FALSE)
  d
}

.tsg_quality_thresholds <- function(x) {
  if (!is.numeric(x) || !all(c("high", "medium", "low") %in% names(x))) {
    stop("`thresholds` must be a named numeric vector containing high, medium, and low.", call. = FALSE)
  }
  x <- x[c("high", "medium", "low")]
  if (anyNA(x) || any(x < 0 | x > 1) || !(x["high"] > x["medium"] && x["medium"] > x["low"])) {
    stop("Thresholds must satisfy 1 >= high > medium > low >= 0.", call. = FALSE)
  }
  x
}
