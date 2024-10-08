% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Download.STPPI.R
\name{Download.STPPI}
\alias{Download.STPPI}
\title{Download.STPPI}
\usage{
Download.STPPI(
  user,
  password,
  dataset_id,
  productType,
  platformSerialIdentifier,
  tileId,
  start,
  end,
  bbox,
  download_path
)
}
\arguments{
\item{user}{User in which it is used in the WEkEO platform}

\item{password}{Password used by the user on the WEkEO platform}

\item{dataset_id}{Database where the product to be downloaded is stored, see WEkEO documentation. In this case use: "EO:EEA:DAT:CLMS_HRVPP_ST".}

\item{productType}{Enter the type of products to download (PPI)}

\item{platformSerialIdentifier}{Select the platform from which the data comes from (example: S2A).}

\item{tileId}{Indicate the tesserae (mosaic) you want to download}

\item{start}{Start date with structure: "yyyy-mm-ddT00:00:00:00.000Z".}

\item{end}{End date with structure: "yyyy-mm-ddT00:00:00:00.000Z".}

\item{bbox}{Coordinates of the study area: Xmin,Ymin,Xmax,Ymax}

\item{download_path}{Directory where the .tif files will be stored}
}
\value{
Set of .tif files related to PPI Seasonal Trajectory(s)
}
\description{
Download.STPPI
}
\examples{
# It is necessary to configure the PATH where python.exe and the hda module are located by creating the object "ruta_python".
ruta_python <- "PATTH/python.exe"
Download.STPPI(
user = "user_name",
password = "Password",
dataset_id = "EO:EEA:DAT:CLMS_HRVPP_ST",
productType = "NDVI",
platformSerialIdentifier = "S2A",
tileId = "30TXL",
start = "2020-01-01T00:00:00.000Z",
end = "2020-01-10T00:00:00.000Z",
bbox = c(-0.89285, 41.48762, -0.86284, 41.50456),
download_path = "local_directory"
)
}
