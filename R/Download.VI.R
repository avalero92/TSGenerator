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
#' # It is necessary to configure the PATH where python.exe and the hda module are located by creating the object "ruta_python".
#' ruta_python <- "PATH/python.exe"
#' Download.VI(user= "Wekeo user", password = "Wekeo password", dataset_id = "EO:EEA:DAT:CLMS_HRVPP_VI", productType = "NDVI",
#' platformSerialIdentifier = "S2A", tileId = "30TXL",
#' start = "2020-01-01T00:00:00.000Z", end = "2020-01-10T00:00:00.000Z", bbox = c(-0.89285, 41.48762, -0.86284, 41.50456),
#' download_path = "C:/Prueba_Fallah", ruta_python = "C:/Python3.9/python.exe")


Download.VI <- function(user, password, dataset_id, productType = NULL,
                        platformSerialIdentifier = NULL, tileId = NULL,
                        start = NULL, end = NULL, bbox = NULL, download_path,
                        ruta_python = NULL) {

  # Validar parámetros obligatorios
  if (missing(user) || missing(password) || missing(dataset_id) || missing(download_path)) {
    stop("Los parámetros user, password, dataset_id y download_path son obligatorios")
  }

  # Verificar que download_path existe
  if (!dir.exists(download_path)) {
    stop("La ruta de descarga no existe: ", download_path)
  }

  # Cargar librería necesaria
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Por favor instala el paquete 'reticulate': install.packages('reticulate')")
  }
  library(reticulate)

  # Configurar Python
  tryCatch({
    if (!is.null(ruta_python)) {
      if (file.exists(ruta_python)) {
        use_python(ruta_python, required = TRUE)
        message("Python configurado correctamente en: ", ruta_python)
      } else {
        stop("La ruta de Python especificada no existe: ", ruta_python)
      }
    }

    # Importar hda
    tryCatch({
      hda <- import("hda")
    }, error = function(e) {
      stop("Error al importar el módulo 'hda'. Asegúrate de que está instalado en Python: ", e$message)
    })

    # Configurar credenciales de usuario
    tryCatch({
      conf <- hda$Configuration(user = user, password = password)
      hda_client <- hda$Client(config = conf)
    }, error = function(e) {
      stop("Error en la configuración del cliente HDA: ", e$message)
    })

    # Preparar los parámetros para la búsqueda
    query <- list(dataset_id = dataset_id)

    # Añadir parámetros opcionales si están definidos
    if (!is.null(productType)) query$productType <- productType
    if (!is.null(platformSerialIdentifier)) query$platformSerialIdentifier <- platformSerialIdentifier
    if (!is.null(tileId)) query$tileId <- tileId
    if (!is.null(start)) query$start <- start
    if (!is.null(end)) query$end <- end
    if (!is.null(bbox)) query$bbox <- bbox

    # Realizar la búsqueda
    mensaje <- paste("Iniciando búsqueda con los siguientes parámetros:",
                     paste(names(query), unlist(query), sep = "=", collapse = ", "))
    message(mensaje)

    matches <- tryCatch({
      hda_client$search(query)
    }, error = function(e) {
      stop("Error en la búsqueda: ", e$message)
    })

    # Mostrar número de resultados encontrados
    num_matches <- length(matches)
    message("Se encontraron ", num_matches, " resultados")

    # Descargar datos si hay resultados
    if (num_matches > 0) {
      message("Descargando datos en: ", download_path)
      result <- tryCatch({
        matches$download(download_path)
        TRUE
      }, error = function(e) {
        warning("Error en la descarga: ", e$message)
        FALSE
      })

      return(list(
        success = result,
        matches = matches,
        count = num_matches
      ))
    } else {
      message("No se encontraron resultados para descargar")
      return(list(
        success = FALSE,
        matches = matches,
        count = 0
      ))
    }

  }, error = function(e) {
    stop("Error general: ", e$message)
  })
}


