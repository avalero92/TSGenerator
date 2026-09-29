% Generated from roxygen source during Phase 5.2; regenerate with roxygen2 before release.
\name{hda_download}\alias{hda_download}\title{Download HDA search results}\usage{
hda_download(
  results, output_dir, force = FALSE, retries = 2L,
  backoff = 2, prompt = interactive()
)
}\arguments{\item{results}{SearchResults returned by \code{hda_search()}.}\item{output_dir}{Destination directory.}\item{force}{Logical; re-download existing files when supported.}\item{retries}{Number of retries after transient failures.}\item{backoff}{Initial retry delay in seconds.}\item{prompt}{Logical; allow \pkg{hdar} to request confirmation.}}\value{The value returned by the \pkg{hdar} download method, invisibly.}\description{Downloads WEkEO HDA search results with controlled retry behaviour.}\seealso{\code{\link{hda_search}}}\concept{WEkEO acquisition}
