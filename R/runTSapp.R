#' Run the TSGenerator graphical interface
#'
#' Launches the unified TSGenerator 2.0 Shiny application. Selected 1.x/early-2.0
#' acquisition mini-apps remain temporarily accessible for compatibility, but new
#' development is concentrated in the unified interface.
#'
#' @param app Character string. Application to launch. The default, `"TSGenerator2"`,
#'   launches the unified 2.0 interface. Compatibility options are `"DownloadSTPPI"`
#'   and `"DownloadVPP"`.
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#'
#' @details
#' The unified application is a graphical layer over the public TSGenerator 2.0 API.
#' Scientific algorithms are not reimplemented in the Shiny server. The historical
#' standalone GAM interface has been retired; missingness modelling remains available
#' through [model_missingness()] and [plot_missingness()].
#'
#' @examples
#' \dontrun{
#' TSGenerator::runTSapp()
#' TSGenerator::runTSapp("DownloadVPP")
#' }
#'
#' @family applications
#' @export
runTSapp <- function(app = "TSGenerator2") {
  active_apps <- c("TSGenerator2", "DownloadSTPPI", "DownloadVPP")
  legacy_apps <- c("DownloadVI", "getCleanVI", "getStack", "renamesVI", "renamesVPP", "GAM")

  if (app %in% legacy_apps) {
    msg <- if (identical(app, "GAM")) {
      paste0(
        "The standalone 'GAM' Shiny interface has been retired. ",
        "Use model_missingness() and plot_missingness(), or the unified TSGenerator2 interface."
      )
    } else {
      paste0(
        "The '", app, "' Shiny interface belongs to the discontinued 1.x VI/QFLAG workflow ",
        "and is not included in the TSGenerator 2.0 active interface."
      )
    }
    stop(msg, call. = FALSE)
  }
  if (!app %in% active_apps) {
    stop(
      paste("Invalid app specified. Choose one of:", paste(active_apps, collapse = ", ")),
      call. = FALSE
    )
  }

  app_dir <- system.file("app", app, package = "TSGenerator")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Could not find the requested TSGenerator application directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
