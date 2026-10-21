\name{Download.HRVPP}\alias{Download.HRVPP}\title{Legacy wrapper for VPP download}
\usage{
Download.HRVPP(
  user = NULL, password = NULL, dataset_id = .VPP_DATASET_ID,
  productType = "SOSD", productGroupId = "s1", tileId = NULL,
  start, end, bbox = NULL, download_path = NULL
)
}
\arguments{
\item{user,password}{Optional legacy WEkEO credentials. Current workflows should use the native authentication configuration.}
\item{dataset_id}{Legacy WEkEO dataset identifier.}
\item{productType}{VPP product type.}
\item{productGroupId}{VPP season/product group identifier.}
\item{tileId}{Optional MGRS tile identifier.}
\item{start,end}{Start and end dates of the search interval.}
\item{bbox}{Optional bounding box.}
\item{download_path}{Explicit destination required for downloads; no default directory is created.}
}
\description{Deprecated TSGenerator 1.x compatibility wrapper. It delegates to \code{download_vpp()} using the native R/hdar backend and no longer requires Python.}
\value{A \code{tsg_vpp_download} object.}
