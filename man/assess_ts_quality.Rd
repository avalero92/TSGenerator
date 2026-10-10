\name{assess_ts_quality}
\alias{assess_ts_quality}
\title{Assess time-series completeness quality}
\usage{
assess_ts_quality(
  data, id_col = "ID", date_col = "Date", value_col = "Value",
  expected_step = NULL, window_days = NULL,
  thresholds = c(high = 0.90, medium = 0.75, low = 0.50),
  start = NULL, end = NULL
)
}
\arguments{
\item{data}{Input time-series data frame.}
\item{id_col,date_col,value_col}{Column names identifying the series, observation date, and value.}
\item{expected_step}{Expected temporal step in days; if \code{NULL}, it is inferred when possible.}
\item{window_days}{Optional assessment-window length in days.}
\item{thresholds}{Named completeness thresholds used to assign quality classes.}
\item{start,end}{Optional start and end dates restricting the assessment period.}
}
\description{Classifies time-series completeness from the proportion of expected dates containing usable values. This metric is separate from Copernicus ST/VPP QFLAG product quality.}
\value{
An object of class \code{tsg_ts_quality} (also a \code{data.frame}) with
one row per series and assessment window. It contains the completeness
columns documented in \code{summarize_missingness()}, a numeric \code{window}
index, and an ordered factor \code{Quality} with levels \code{Very low},
\code{Low}, \code{Medium}, and \code{High}, based on
\code{value_completeness}. Attributes \code{thresholds} and
\code{window_days} record the assessment settings.
}
