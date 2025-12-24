#' Run Shiny applications from the TSGenerator package
#'
#' This function launches different Shiny applications that are part of the TSGenerator package.
#' Each application provides a graphical interface for performing different operations
#' related to time series.
#'
#' @import shiny
#' @name runTSapp
#' @param app Character string. Specifies which Shiny application to run.
#'   Valid options: "DownloadVI", "DownloadSTPPI", "DownloadVPP", "getCleanVI",
#'   "getStack", "renamesVI", "renamesVPP", "GAM". Por defecto es "DownloadVI".
#'
#' @return It does not return a value, but runs a Shiny application in the default web browser.
#'
#' @details
#' The available applications are:
#' \itemize{
#'   \item \code{DownloadVI}: Interface for downloading VI data (NDVI, fAPAR, LAI and PPI).
#'   \item \code{DownloadSTPPI}: Interface for downloading PPI seasonal trajectory data (ST).
#'   \item \code{DownloadVPP}: Interface for downloading VPP data.
#'   \item \code{getCleanVI}: Data cleaning and processing tool.
#'   \item \code{getStack}: Tool for stacking data sets.
#'   \item \code{renamesVI}: VI file renaming utility.
#'   \item \code{renamesVPP}: Utility to rename VPP files.
#'   \item \code{GAM}: GAM model to analysis NA data
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the DownloadVI application (default)
#' TSGenerator::runTSapp()
#'
#' # Explicitly specify the DownloadVI application
#' TSGenerator::runTSapp("DownloadVI")
#'
#' # Launch the getClean application
#' TSGenerator::runTSapp("getCleanVI")
#' }
#'
#' @importFrom shiny runApp
#' @export
runTSapp <- function(app = "DownloadVI") {
  valid_apps <- c("DownloadVI", "DownloadSTPPI", "DownloadVPP",
                  "getCleanVI", "getStack", "renamesVI", "renamesVPP", "GAM")
  if (!app %in% valid_apps) {
    stop(paste("Invalid app specified. Please choose one of the following options:",
               paste(valid_apps, collapse = ", ")), call. = FALSE)
  }

  appDirBase <- system.file(package = "TSGenerator")
  appDir <- file.path(appDirBase, "app", app)
  cat("Directorio de la aplicación:", appDir, "\n")
  if (!dir.exists(appDir)) {
    stop("Could not find application directory ).", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}


