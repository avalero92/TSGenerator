\name{download_vpp}\alias{download_vpp}\title{Download Copernicus HR-VPP VPP parameters}
\usage{download_vpp(start, end, output_dir = "HRVPP_VPP",
  product = c("SOSD", "MAXD", "EOSD", "LENGTH"), season = "s1",
  tile_id = NULL, bbox = NULL, platform = "S2A, S2B", product_version = NULL,
  client = NULL, download = TRUE, limit = NULL, overwrite = FALSE,
  prompt = interactive(), quiet = FALSE, retries = 2L, backoff = 2)}
\arguments{
\item{start,end}{Start and end dates of the WEkEO search interval.}
\item{output_dir}{Directory for downloaded files.}
\item{product}{VPP parameter or parameters to query.}
\item{season}{VPP season identifier.}
\item{tile_id}{Optional MGRS tile identifier.}
\item{bbox}{Optional bounding box used to spatially restrict the query.}
\item{platform}{Sentinel-2 platform identifier(s).}
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
\description{Searches and optionally downloads HR-VPP phenology and productivity parameters through WEkEO HDA V2 using the native R \pkg{hdar} backend.}
\value{An object of class \code{tsg_vpp_download}.}
