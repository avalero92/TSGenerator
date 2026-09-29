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
