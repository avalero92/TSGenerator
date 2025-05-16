#' Ejecuta aplicaciones Shiny del paquete TSGenerator
#'
#' Esta función lanza diferentes aplicaciones Shiny que forman parte del paquete TSGenerator.
#' Cada aplicación proporciona una interfaz gráfica para realizar diferentes operaciones
#' relacionadas con series temporales.
#'
#' @import shiny
#' @name runTSapp
#' @param app Cadena de caracteres. Especifica qué aplicación Shiny ejecutar.
#'   Opciones válidas: "DownloadVI", "DownloadSTPPI", "DownloadVPP", "getClean",
#'   "getStack", "renamesVI", "renamesVPP". Por defecto es "DownloadVI".
#'
#' @return No devuelve un valor, pero ejecuta una aplicación Shiny en el navegador web predeterminado.
#'
#' @details
#' Las aplicaciones disponibles son:
#' \itemize{
#'   \item \code{DownloadVI}: Interfaz para descargar datos de VI (Vegetación e Índices).
#'   \item \code{DownloadSTPPI}: Interfaz para descargar datos de STPPI.
#'   \item \code{DownloadVPP}: Interfaz para descargar datos de VPP.
#'   \item \code{getClean}: Herramienta para limpiar y procesar datos.
#'   \item \code{getStack}: Herramienta para apilar conjuntos de datos.
#'   \item \code{renamesVI}: Utilidad para renombrar archivos VI.
#'   \item \code{renamesVPP}: Utilidad para renombrar archivos VPP.
#' }
#'
#' @examples
#' \dontrun{
#' # Lanzar la aplicación DownloadVI (predeterminada)
#' TSGenerator::runApp()
#'
#' # Especificar explícitamente la aplicación DownloadVI
#' TSGenerator::runApp("DownloadVI")
#'
#' # Lanzar la aplicación getClean
#' TSGenerator::runApp("getClean")
#' }
#'
#' @importFrom shiny runApp
#' @export
runTSapp <- function(app = "DownloadVI") {
  valid_apps <- c("DownloadVI", "DownloadSTPPI", "DownloadVPP",
                  "getClean", "getStack", "renamesVI", "renamesVPP")
  if (!app %in% valid_apps) {
    stop(paste("Invalid app specified. Please choose one of the following options:",
               paste(valid_apps, collapse = ", ")), call. = FALSE)
  }

  # Para versión instalada
  appDir <- system.file("inst"/"applications", app, package = "TSGenerator")
  cat("Directorio de la aplicación:", appDir, "\n")  # Mensaje de depuración
  if (appDir == "" || !dir.exists(appDir)) {
    stop("Could not find application directory. Try re-installing `TSGenerator`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}


