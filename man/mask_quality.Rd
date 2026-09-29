\name{mask_quality}\alias{mask_quality}\title{Mask raster values using HR-VPP quality flags}
\usage{
mask_quality(
  x,
  quality,
  type = c("ST", "VPP"),
  min_quality = NULL,
  keep_codes = NULL
)
}
\arguments{
\item{x}{Target \code{terra::SpatRaster}.}
\item{quality}{Quality \code{terra::SpatRaster} with matching geometry.}
\item{type}{Quality-flag family, either \code{"ST"} or \code{"VPP"}.}
\item{min_quality}{Optional minimum quality code.}
\item{keep_codes}{Optional explicit quality codes to retain; overrides \code{min_quality}.}
}
\value{A masked \code{terra::SpatRaster}.}
\description{Masks values according to explicit quality rules. Categorical QFLAG rasters are never resampled automatically.}
\seealso{\code{\link{quality_info}}, \code{\link{classify_quality}}}
\concept{product quality}
