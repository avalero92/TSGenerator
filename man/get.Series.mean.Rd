\name{get.Series.mean}
\alias{get.Series.mean}
\title{Legacy mean time-series extractor}
\usage{get.Series.mean(pathRaster = NULL, shapefile = NULL, factorR = NULL)}
\arguments{\item{pathRaster}{Directory containing TIFF rasters.}\item{shapefile}{Polygon sf/sfc or terra SpatVector.}\item{factorR}{Legacy numeric scale divisor.}}
\value{A data frame with legacy columns ID, Date, and Mean.}
\description{Deprecated compatibility wrapper around \code{extract_ts(..., fun = "mean")}.}
