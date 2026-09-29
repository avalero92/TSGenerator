\name{extract_vpp}
\alias{extract_vpp}
\title{Extract HR-VPP phenology and productivity parameters}
\usage{extract_vpp(x, polygons, id_col = NULL, product = NULL, season = NULL,
  year = NULL, fun = "median", transform = TRUE, na.rm = TRUE,
  exact = FALSE, touches = FALSE)}
\arguments{
\item{x}{VPP raster input, file paths, or directory accepted by TSGenerator.}
\item{polygons}{Polygon \code{sf}/\code{sfc} object or \code{terra::SpatVector}.}
\item{id_col}{Optional polygon identifier column.}
\item{product}{Optional VPP product filter.}
\item{season}{Optional season filter.}
\item{year}{Optional year filter.}
\item{fun}{Polygon summary function.}
\item{transform}{Logical; transform polygons to raster CRS when required.}
\item{na.rm}{Logical; remove missing raster values in summaries.}
\item{exact}{Logical passed to the extraction backend for exact polygon extraction.}
\item{touches}{Logical controlling inclusion of touched cells.}
}
\description{Extracts polygon statistics from Copernicus HR-VPP VPP rasters using product-aware decoding, scaling, NoData handling and season metadata.}
\value{A data frame of class \code{tsg_vpp}. Date products populate \code{Date}; numeric products populate \code{Value}.}
