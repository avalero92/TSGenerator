\name{count_missing}
\alias{count_missing}
\title{Summarize missing observations through time and phenological intervals}
\usage{
count_missing(
  data, sos, maxd, eos, doy_col = "DOY", year_col = "Year",
  na_col = "NDVI", fid_col = "ID"
)
}
\arguments{\item{data}{A data frame containing the time series.}\item{sos,maxd,eos}{Day-of-year thresholds.}\item{doy_col,year_col,na_col,fid_col}{Column names.}}
\value{A list containing an interactive plot, a missing-count table, and interval totals.}
\description{Counts missing observations by day and year and summarizes them relative to phenological intervals.}
