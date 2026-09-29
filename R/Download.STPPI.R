#' Legacy wrapper for Seasonal Trajectories download
#'
#' `Download.STPPI()` is retained for compatibility with TSGenerator 1.x.
#' It now delegates to [download_st()] and no longer requires Python or
#' `reticulate`. New code should call `download_st()` directly.
#'
#' @param user,password Optional WEkEO credentials. If omitted, `hdar` reads
#'   credentials from `~/.hdarc`.
#' @param dataset_id Legacy dataset identifier. Ignored after validation because
#'   `download_st()` uses the supported ST dataset.
#' @param productType ST product (`PPI` or `QFLAG`).
#' @param platformSerialIdentifier Platform filter.
#' @param tileId Sentinel-2 tile identifier.
#' @param start,end Date/time range.
#' @param bbox Optional EPSG:4326 bounding box.
#' @param download_path Output directory.
#' @return A `tsg_st_download` object.
#' @export
Download.STPPI <- function(user = NULL, password = NULL,
                           dataset_id = .ST_DATASET_ID,
                           productType = "PPI",
                           platformSerialIdentifier = "S2A, S2B",
                           tileId = NULL, start, end, bbox = NULL,
                           download_path = "HRVPP_ST") {
  warning("'Download.STPPI()' is deprecated; use 'download_st()'. The legacy wrapper now uses the native R/hdar backend.", call. = FALSE)
  if (!identical(dataset_id, .ST_DATASET_ID)) {
    warning("Legacy 'dataset_id' is ignored; using the supported ST dataset: ", .ST_DATASET_ID, call. = FALSE)
  }
  client <- hda_client(username = user, password = password)
  download_st(start = start, end = end, output_dir = download_path,
              product = productType, tile_id = tileId, bbox = bbox,
              platform = platformSerialIdentifier, client = client)
}
