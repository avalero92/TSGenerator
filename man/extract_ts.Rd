\name{extract_ts}
\alias{extract_ts}
\title{Extract polygon-level raster time series}
\usage{
extract_ts(x, polygons, id_col = NULL, fun = "median", dates = NULL,
  transform = TRUE, scale_factor = 1, offset = 0, na.rm = TRUE,
  exact = FALSE, touches = FALSE)
}
\arguments{
\item{x}{A terra SpatRaster, TIFF path, directory, or vector of TIFF paths.}
\item{polygons}{An sf/sfc object or terra SpatVector containing polygons.}
\item{id_col}{Optional unique polygon ID column.}
\item{fun}{Summary function; mean, median, min, max, sum, or a function.}
\item{dates}{Optional vector with one date per raster layer.}
\item{transform}{Transform polygons to raster CRS when needed.}
\item{scale_factor}{Numeric divisor applied after extraction.}
\item{offset}{Numeric offset added after scaling.}
\item{na.rm}{Remove missing raster values during summarisation.}
\item{exact}{Use exact polygon-cell fractions when supported.}
\item{touches}{Include cells touched by polygon boundaries.}
}
\value{A tidy data frame of class \code{tsg_time_series} with ID, Date, Layer and Value columns.}
\description{Terra-first replacement for the duplicated TSGenerator 1.x mean and median extraction workflows.}
