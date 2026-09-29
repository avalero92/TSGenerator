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
