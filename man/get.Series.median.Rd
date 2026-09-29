\name{get.Series.median}
\alias{get.Series.median}
\title{Legacy median time-series extractor}
\usage{get.Series.median(pathRaster = NULL, shapefile = NULL, factorR = NULL)}
\arguments{\item{pathRaster}{Directory containing TIFF rasters.}\item{shapefile}{Polygon sf/sfc or terra SpatVector.}\item{factorR}{Legacy numeric scale divisor.}}
\value{A data frame with legacy columns ID, Date, and Mean. The historical column name is preserved.}
\description{Deprecated compatibility wrapper around \code{extract_ts(..., fun = "median")}.}
