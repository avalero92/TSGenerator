\name{impute_ts}
\alias{impute_ts}
\title{Impute missing values in vegetation time series}
\usage{
impute_ts(data, id_col = "ID", date_col = "Date", value_col = "Value",
  method = c("linear", "kalman"), series_type = c("user", "raw", "st"),
  allow_processed = FALSE, complete_grid = FALSE, expected_step = NULL,
  max_gap = NULL, allow_edge = FALSE, output_col = NULL)
}
\arguments{
\item{data}{Input time-series data frame.}
\item{id_col,date_col,value_col}{Column names identifying the series, observation date, and value.}
\item{method}{Imputation method.}
\item{series_type}{Series provenance: user, raw, or processed ST.}
\item{allow_processed}{Logical; explicitly permit additional imputation of processed ST series.}
\item{complete_grid}{Logical; complete the expected temporal grid before imputation.}
\item{expected_step}{Expected temporal step in days.}
\item{max_gap}{Optional maximum gap eligible for imputation.}
\item{allow_edge}{Logical; permit filling gaps at series edges where supported.}
\item{output_col}{Optional name for the imputed output column.}
}
\description{Explicit, opt-in imputation with provenance and safeguards for processed Seasonal Trajectories.}
\value{A data frame of class \code{tsg_imputed_series}.}
