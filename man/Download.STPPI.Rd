\name{Download.STPPI}\alias{Download.STPPI}\title{Legacy wrapper for Seasonal Trajectories download}
\usage{
Download.STPPI(
  user = NULL, password = NULL, dataset_id = .ST_DATASET_ID,
  productType = "PPI", platformSerialIdentifier = "S2A, S2B",
  tileId = NULL, start, end, bbox = NULL, download_path = "HRVPP_ST"
)
}
\arguments{
\item{user,password}{Optional legacy WEkEO credentials. Current workflows should use the native authentication configuration.}
\item{dataset_id}{Legacy WEkEO dataset identifier.}
\item{productType}{Seasonal Trajectories product type.}
\item{platformSerialIdentifier}{Sentinel-2 platform identifier(s).}
\item{tileId}{Optional MGRS tile identifier.}
\item{start,end}{Start and end dates of the search interval.}
\item{bbox}{Optional bounding box.}
\item{download_path}{Directory in which downloaded files are stored.}
}
\description{Deprecated TSGenerator 1.x compatibility wrapper. It delegates to \code{download_st()} using the native R/hdar backend and no longer requires Python.}
\value{A \code{tsg_st_download} object.}
