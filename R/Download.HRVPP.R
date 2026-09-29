#' Legacy wrapper for HR-VPP VPP download
#'
#' `Download.HRVPP()` is retained for compatibility with TSGenerator 1.x.
#' It now delegates to [download_vpp()] and no longer requires Python or
#' `reticulate`. New code should call `download_vpp()` directly.
#'
#' @param user,password Optional WEkEO credentials. If omitted, `hdar` reads
#'   credentials from `~/.hdarc`.
#' @param dataset_id Legacy dataset identifier.
#' @param productType VPP parameter, e.g. `SOSD`, `EOSD`, `TPROD`.
#' @param productGroupId Season (`s1` or `s2`).
#' @param tileId Sentinel-2 tile identifier.
#' @param start,end Date/time range.
#' @param bbox Optional EPSG:4326 bounding box.
#' @param download_path Output directory.
#' @return A `tsg_vpp_download` object.
#' @export
Download.HRVPP <- function(user = NULL, password = NULL,
                           dataset_id = .VPP_DATASET_ID,
                           productType = "SOSD", productGroupId = "s1",
                           tileId = NULL, start, end, bbox = NULL,
                           download_path = "HRVPP_VPP") {
  warning("'Download.HRVPP()' is deprecated; use 'download_vpp()'. The legacy wrapper now uses the native R/hdar backend.", call. = FALSE)
  if (!identical(dataset_id, .VPP_DATASET_ID)) {
    warning("Legacy 'dataset_id' is ignored; using the supported VPP dataset: ", .VPP_DATASET_ID, call. = FALSE)
  }
  client <- hda_client(username = user, password = password)
  download_vpp(start = start, end = end, output_dir = download_path,
               product = productType, season = productGroupId,
               tile_id = tileId, bbox = bbox, client = client)
}
