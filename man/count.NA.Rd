% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count.NA.R
\name{count.NA}
\alias{count.NA}
\title{count.NA}
\usage{
\method{count}{`NA`}(
  data,
  sos,
  maxd,
  eos,
  doy_col = "DOY",
  year_col = "Year",
  na_col = "NDVI",
  fid_col = "ID"
)
}
\arguments{
\item{data}{Dataframe where the variables DOY; Year; variable of interest and identifying variable that stores the identifiers of each polygon/plot are stored.}

\item{sos}{Opening day of the season}

\item{maxd}{Maximum growth day}

\item{eos}{End of season day}

\item{doy_col}{Column storing Julian days}

\item{year_col}{Column that stores the years in \%yyyyy format}

\item{na_col}{Variable of interest}

\item{fid_col}{Column that stores the identifiers of each polygon/plot.}
}
\value{
A list type object that stores two dataframes: 1) Total NA by days and by polygons/plots; 2) Total NA by days and plots and the critical periods of the vegetation.
}
\description{
count.NA
}
\examples{
sos <- 47  # Replace with your real value
maxd <- 105  # Replace with your real value
eos <- 151  # Replace with your real value
Call the function with your data
result <- count.NA(datos, sos, maxd, eos,doy_col = "DOY", year_col = "Year", na_col = "NDVI_median", fid_col = "fid")
print(result$interactive_plot)
}
