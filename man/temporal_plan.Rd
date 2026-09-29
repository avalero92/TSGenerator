% Generated from roxygen source during Phase 5.2; regenerate with roxygen2 before release.
\name{temporal_plan}\alias{temporal_plan}\title{Inspect and validate a vegetation time series}\usage{
temporal_plan(
  data, id_col = "ID", date_col = "Date", value_col = "Value",
  expected_step = NULL, allow_duplicates = FALSE
)
}\arguments{\item{data}{A data frame containing one or more time series.}\item{id_col}{Series identifier column.}\item{date_col}{Date column.}\item{value_col}{Value column.}\item{expected_step}{Optional expected temporal step in days.}\item{allow_duplicates}{Logical; allow duplicate ID-date observations.}}\value{An object of class \code{tsg_temporal_plan} containing validated data and diagnostics.}\description{Validates IDs, dates, missing values, duplicate observations, and temporal regularity without modifying the series.}\seealso{\code{\link{summarize_missingness}}, \code{\link{assess_ts_quality}}, \code{\link{impute_ts}}}\concept{time-series analysis}
