\name{count_missing}
\alias{count_missing}
\title{Legacy phenology-specific missing-count interface}
\usage{
count_missing(
  data, sos, maxd, eos, doy_col = "DOY", year_col = "Year",
  na_col = "NDVI", fid_col = "ID"
)
}
\arguments{\item{data}{A data frame containing the time series.}\item{sos,maxd,eos}{Day-of-year thresholds.}\item{doy_col,year_col,na_col,fid_col}{Column names.}}
\value{No return value. This retired function signals an error with migration guidance; use \code{summarize_missingness()} for missingness counts and \code{assess_ts_quality()} for completeness classes.}
\description{Retired TSGenerator 1.x interface. The former phenology-specific missing-count workflow is no longer executed.}
