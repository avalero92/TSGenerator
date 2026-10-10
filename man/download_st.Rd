\name{download_st}\alias{download_st}\title{Download Copernicus HR-VPP Seasonal Trajectories}
\usage{download_st(start, end, output_dir = NULL, product = "PPI",
  tile_id = NULL, bbox = NULL, platform = "S2A, S2B", resolution = 10L,
  product_version = NULL, client = NULL, download = TRUE, limit = NULL,
  overwrite = FALSE, prompt = interactive(), quiet = FALSE, retries = 2L,
  backoff = 2)}
\arguments{
\item{start,end}{Start and end dates of the WEkEO search interval.}
\item{output_dir}{Explicit destination required when \code{download = TRUE}; no default directory is created. Ignored in preview mode.}
\item{product}{Seasonal Trajectories product type.}
\item{tile_id}{Optional MGRS tile identifier.}
\item{bbox}{Optional bounding box used to spatially restrict the query.}
\item{platform}{Sentinel-2 platform identifier(s).}
\item{resolution}{Requested spatial resolution in metres.}
\item{product_version}{Optional HR-VPP product version.}
\item{client}{Optional preconfigured native HDA client.}
\item{download}{Logical; download results when \code{TRUE}, otherwise return a preview.}
\item{limit}{Optional maximum number of search results.}
\item{overwrite}{Logical; overwrite existing files.}
\item{prompt}{Logical; allow interactive download confirmation.}
\item{quiet}{Logical; suppress progress messages when possible.}
\item{retries}{Number of download retries.}
\item{backoff}{Backoff interval used between retries.}
}
\description{Searches and optionally downloads HR-VPP Seasonal Trajectories through WEkEO HDA V2 using the native R \pkg{hdar} backend.}
\value{An object of class \code{tsg_st_download}.}
