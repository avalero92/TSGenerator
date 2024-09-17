#' Download.HRVPP
#'
#' @import reticulate
#' @name Download.HRVPP
#'
#' @param user
#' User in which it is used in the WEkEO platform
#' @param password
#' Password used by the user on the WEkEO platform
#' @param dataset_id
#' Database where the product to be downloaded is stored, see WEkEO documentation. En el cso de los productos HRVPP usar: "EO:EEA:DAT:CLMS_HRVPP_VPP"
#' @param productType
#' Select the type of product to download (example: Start of Season_date(SOSD)). For other product types see WEkEO documentation.
#' @param tileId
#' ndicate the tesserae (mosaic) you want to download
#' @param productGroupID
#' The product group will be selected, which can be Season 1 or 2 (example: s2).
#' @param start
#' Start date with structure: "yyyy-mm-ddT00:00:00:00.000Z".
#' @param end
#' End date with structure: "yyyy-mm-ddT00:00:00:00.000Z".
#' @param bbox
#' Coordinates of the study area: Xmin,Ymin,Xmax,Ymax
#' @param download_path
#' Directory where the .tif files will be stored
#'
#' @return
#' Set of .tif files relating to the selected HRVPP product(s)
#'
#' @export
#'
#' @examples
#' #' Download.HRVPP (
#'user = "user_name",
#'password = "Password",
#'dataset_id = "EO:EEA:DAT:CLMS_HRVPP_VPP",
#'productType = "SOSD",
#'productGroupId = "s1",
#'tileId = "30TXL",
#'start = "2020-01-01T00:00:00.000Z",
#'end = "2020-01-10T00:00:00.000Z",
#'bbox = c(-0.89285, 41.48762, -0.86284, 41.50456),
#'download_path = "local_directory"
#')

Download.HRVPP <- function(user, password, dataset_id, productType,productGroupId,
                              tileId, start, end, bbox, download_path) {

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
    productGroupId = productGroupId,
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
