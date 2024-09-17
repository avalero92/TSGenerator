# Function to download Vegetation index data using hda
#' Download.VI
#'
#' @name Download.VI
#'
#' @param user
#' User in which it is used in the WEkEO platform
#' @param password
#' Password used by the user on the WEkEO platform
#' @param dataset_id
#' Database where the product to be downloaded is stored, see WEkEO documentation.
#' @param productType Enter the type of products to download (e.g. NDVI, PPI, FAPAR or LAI). The QFLAG quality product must be downloaded separately from the VI.
#' @param platformSerialIdentifier Select the platform from which the data comes from (example: S2A).
#' @param tileId Indicate the tesserae (mosaic) you want to download
#' @param start Start date with structure: "yyyy-mm-ddT00:00:00:00.000Z".
#' @param end End date with structure: "yyyy-mm-ddT00:00:00:00.000Z".
#' @param bbox Coordinates of the study area: Xmin,Ymin,Xmax,Ymax
#' @param download_path Directory where the .tif files will be stored
#'
#' @return Set of .tif files relating to the selected VI(s)
#' @export
#'
#' @examples
#' Download.VI(
#'user = "user_name",
#'password = "Password",
#'dataset_id = "EO:EEA:DAT:CLMS_HRVPP_VI",
#'productType = "NDVI",
#'platformSerialIdentifier = "S2A",
#'tileId = "30TXL",
#'start = "2020-01-01T00:00:00.000Z",
#'end = "2020-01-10T00:00:00.000Z",
#'bbox = c(-0.89285, 41.48762, -0.86284, 41.50456),
#'download_path = "local_directory"
#')
Download.VI <- function(user, password, dataset_id, productType,
                              platformSerialIdentifier, tileId, start, end, bbox, download_path) {

  library(reticulate)

  # Importar hda
  hda <- import("hda")

  # Configure user credentials
  conf <- hda$Configuration(user = user, password = password)
  hda_client <- hda$Client(config = conf)

  # Select the parameters for downloading
  query <- list(
    dataset_id = dataset_id,
    productType = productType,
    platformSerialIdentifier = platformSerialIdentifier,
    tileId = tileId,
    start = start,
    end = end,
    bbox = bbox
  )

  # Send your request
  matches <- hda_client$search(query)
  print(matches)

  # Download data in the specified path
  matches$download(download_path)
}
