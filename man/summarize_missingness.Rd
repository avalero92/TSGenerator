\name{summarize_missingness}
\alias{summarize_missingness}
\title{Summarise missingness and temporal completeness}
\usage{
summarize_missingness(
  data, id_col = "ID", date_col = "Date", value_col = "Value",
  expected_step = NULL, start = NULL, end = NULL
)
}
\arguments{
\item{data}{Input time-series data frame.}
\item{id_col,date_col,value_col}{Column names identifying the series, observation date, and value.}
\item{expected_step}{Expected temporal step in days; if \code{NULL}, it is inferred when possible.}
\item{start,end}{Optional start and end dates restricting the summary period.}
}
\description{Separates absent expected dates from explicit NA values and reports temporal and usable-value completeness for each series.}
\value{
An object of class \code{tsg_missingness} (also a \code{data.frame}) with
one row per series. Columns are the identifier (named by \code{id_col}),
\code{start}, \code{end}, \code{expected_step_days}, \code{n_expected},
\code{n_observed_dates}, \code{n_absent_dates}, \code{n_value_na},
\code{n_usable}, \code{date_completeness}, \code{value_completeness},
and \code{missing_fraction}. Counts distinguish absent expected dates from
observed dates with missing values. Fractions use \code{n_expected} as the
denominator and are \code{NA} when that count is zero.
}
