#' Model temporal missingness with a generalized additive model
#'
#' Summarises missing observations by year and day-of-year and fits a binomial
#' GAM to the missing/available counts. Year is treated as a factor by default,
#' avoiding the unintended linear year effect in the TSGenerator 1.x model.
#'
#' @param data Data frame containing repeated observations.
#' @param year_col,doy_col,value_col Column names for year, day-of-year and the
#'   variable whose missingness is modelled.
#' @param k Basis dimension for the cyclic day-of-year smooth. NULL lets mgcv
#'   choose its default.
#' @param year_effect One of "factor" (default) or "none".
#' @param cyclic Logical; use a cyclic cubic regression spline for DOY.
#' @return An object of class `tsg_missingness_model` containing the fitted GAM,
#'   observed/predicted missingness table and model settings.
#' @family time-series analysis
#' @export
model_missingness <- function(data, year_col = "Year", doy_col = "DOY",
                              value_col = "Value", k = NULL,
                              year_effect = c("factor", "none"), cyclic = TRUE) {
  year_effect <- match.arg(year_effect)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  req <- c(year_col, doy_col, value_col)
  miss <- setdiff(req, names(data))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!requireNamespace("mgcv", quietly = TRUE)) stop("Package `mgcv` is required.", call. = FALSE)

  yr <- data[[year_col]]; dy <- suppressWarnings(as.numeric(data[[doy_col]]))
  if (anyNA(yr) || anyNA(dy)) stop("Year and DOY cannot contain missing/non-numeric values.", call. = FALSE)
  if (any(dy < 1 | dy > 366)) stop("DOY must be between 1 and 366.", call. = FALSE)

  d <- data.frame(.Year = yr, .DOY = dy, .Missing = is.na(data[[value_col]]))
  agg <- stats::aggregate(.Missing ~ .Year + .DOY, data = d,
                          FUN = function(z) c(Missing = sum(z), Total = length(z)))
  counts <- data.frame(Year = agg$.Year, DOY = agg$.DOY,
                       Missing = agg$.Missing[, "Missing"], Total = agg$.Missing[, "Total"])
  counts$Available <- counts$Total - counts$Missing
  counts$MissingProportion <- counts$Missing / counts$Total
  counts$YearFactor <- factor(counts$Year)

  bs <- if (isTRUE(cyclic)) "cc" else "cs"
  smooth <- if (is.null(k)) sprintf("s(DOY, bs='%s')", bs) else sprintf("s(DOY, bs='%s', k=%d)", bs, as.integer(k))
  rhs <- if (year_effect == "factor") paste(smooth, "+ YearFactor") else smooth
  f <- stats::as.formula(paste("cbind(Missing, Available) ~", rhs))
  knots <- if (isTRUE(cyclic)) list(DOY = c(0.5, 366.5)) else NULL
  fit <- mgcv::gam(f, family = stats::binomial(link = "logit"), data = counts,
                   knots = knots, method = "REML")
  counts$Predicted <- as.numeric(stats::predict(fit, newdata = counts, type = "response"))

  structure(list(data = counts, model = fit, year_effect = year_effect,
                 cyclic = isTRUE(cyclic), k = k, columns = list(year = year_col, doy = doy_col, value = value_col)),
            class = "tsg_missingness_model")
}

#' @family time-series analysis
#' @export
print.tsg_missingness_model <- function(x, ...) {
  cat("TSGenerator missingness model\n")
  cat("Rows:", nrow(x$data), "\n")
  cat("Year effect:", x$year_effect, "\n")
  cat("DOY smooth:", if (x$cyclic) "cyclic" else "non-cyclic", "\n")
  invisible(x)
}

#' Plot observed and modelled temporal missingness
#'
#' @param x A `tsg_missingness_model` or compatible data frame.
#' @param sos,max_doy,eos Optional phenological DOY markers.
#' @param facet Logical; facet by year.
#' @return A ggplot object. The function does not print it automatically.
#' @family time-series analysis
#' @export
plot_missingness <- function(x, sos = NULL, max_doy = NULL, eos = NULL, facet = TRUE) {
  d <- if (inherits(x, "tsg_missingness_model")) x$data else x
  if (!is.data.frame(d)) stop("`x` must be a missingness model or data frame.", call. = FALSE)
  req <- c("Year", "DOY", "MissingProportion", "Predicted")
  miss <- setdiff(req, names(d)); if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = DOY, y = MissingProportion, group = factor(Year))) +
    ggplot2::geom_point(ggplot2::aes(color = factor(Year)), alpha = 0.65) +
    ggplot2::geom_line(ggplot2::aes(y = Predicted, color = factor(Year)), linewidth = 0.9) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(x = "Day of year", y = "Missing observations", color = "Year") + ggplot2::theme_bw()
  if (isTRUE(facet)) p <- p + ggplot2::facet_wrap(~Year)
  marks <- c(SOS = sos, MAX = max_doy, EOS = eos); marks <- marks[!is.na(marks) & !vapply(marks, is.null, logical(1))]
  if (length(marks)) {
    md <- data.frame(x = as.numeric(marks), label = names(marks))
    p <- p + ggplot2::geom_vline(data = md, ggplot2::aes(xintercept = x), linetype = "dashed", inherit.aes = FALSE) +
      ggplot2::geom_text(data = md, ggplot2::aes(x = x, y = 1, label = label), angle = 90, vjust = -0.25, inherit.aes = FALSE)
  }
  p
}
