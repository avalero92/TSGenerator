% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.CleanIV.R
\name{get.Clean.IV}
\alias{get.Clean.IV}
\title{get.Clean.IV}
\usage{
get.Clean.IV(stack_folder = NULL, output_folder)
}
\arguments{
\item{stack_folder}{Path to the directory containing the raster files (.tif).
If NULL, an error message will be displayed.}

\item{output_folder}{ath to the directory where the processed raster files will be saved.}
}
\value{
It does not return any value, but saves the processed raster files in the output directory.
}
\description{
Obtain and clean raster images
}
\details{
This function processes a set of raster files in a specified directory,,
removing pixels in the first band according to the values in the second band.
}
\examples{
Calling the function with one input and one output directory
get.Clean.IV(stack_folder = "ruta/a/tu/directorio", output_folder = "ruta/a/directorio/salida")
}
