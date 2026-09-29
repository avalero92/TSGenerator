\name{model_missingness}
\alias{model_missingness}
\title{Model temporal missingness with a generalized additive model}
\usage{
model_missingness(
  data, year_col = "Year", doy_col = "DOY", value_col = "Value",
  k = NULL, year_effect = c("factor", "none"), cyclic = TRUE
)
}
\arguments{\item{data}{Data frame containing repeated observations.}\item{year_col,doy_col,value_col}{Column names.}\item{k}{Optional GAM basis dimension.}\item{year_effect}{Factor year effect or none.}\item{cyclic}{Use a cyclic DOY smooth.}}
\value{An object of class \code{tsg_missingness_model}.}
\description{Aggregates missing/available counts and fits a binomial GAM. Year is treated as a factor by default.}
