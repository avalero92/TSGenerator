\name{count.NA}
\alias{count.NA}
\title{Count missing observations (legacy name)}
\usage{
count.NA(
  data, sos, maxd, eos, doy_col = "DOY", year_col = "Year",
  na_col = "NDVI", fid_col = "ID"
)
}
\arguments{
\item{data}{Input data frame.}
\item{sos,maxd,eos}{Phenological day-of-year boundaries.}
\item{doy_col,year_col,na_col,fid_col}{Column names for day of year, year, assessed value, and feature identifier.}
}
\description{Compatibility wrapper for \code{count_missing()}. New code should use \code{count_missing()}.}
\value{
No return value. This retired compatibility function signals an error with
migration guidance; use \code{summarize_missingness()} instead.
}
