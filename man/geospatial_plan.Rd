% Generated from roxygen source during Phase 5.2; regenerate with roxygen2 before release.
\name{geospatial_plan}\alias{geospatial_plan}\title{Inspect inputs for the TSGenerator 2.0 geospatial engine}\usage{
geospatial_plan(
  x, polygons, id_col = NULL, transform = TRUE,
  scale_factor = 1, offset = 0
)
}\arguments{\item{x}{A \code{terra::SpatRaster}, TIFF path, directory, or vector of TIFF paths.}\item{polygons}{An \code{sf}/\code{sfc} object or \code{terra::SpatVector} of polygons.}\item{id_col}{Optional unique polygon ID column.}\item{transform}{Logical; transform polygons to the raster CRS when required.}\item{scale_factor}{Numeric divisor applied after extraction.}\item{offset}{Numeric offset applied after scaling.}}\value{An object of class \code{tsg_geospatial_plan}; no extraction is performed.}\description{Validates raster/vector geometry, CRS, IDs, and scaling before terra-based extraction.}\seealso{\code{\link{extract_ts}}, \code{\link{extract_vpp}}}\concept{geospatial core}
