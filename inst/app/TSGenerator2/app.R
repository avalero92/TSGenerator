# TSGenerator 2.0 unified Shiny application
# Phase 5.7.1: application shell, dashboard, navigation and core diagnostics.

library(shiny)
library(shinydashboard)

# Upload limit for browser-based fileInput() transfers.
# Existing files / directory bypasses HTTP upload and is preferred for large rasters.
# Respect a user-specified shiny.maxRequestSize; otherwise use a configurable default.
if (is.null(getOption("shiny.maxRequestSize"))) {
  upload_mib <- suppressWarnings(as.numeric(Sys.getenv("TSG_SHINY_UPLOAD_MAX_MIB", "512")))
  if (length(upload_mib) != 1L || !is.finite(upload_mib) || upload_mib <= 0) {
    stop("TSG_SHINY_UPLOAD_MAX_MIB must be a positive finite number.", call. = FALSE)
  }
  options(shiny.maxRequestSize = upload_mib * 1024^2)
}

# Derive the visible version from the loaded package (also works with pkgload::load_all()).
# Stop rather than silently advertising an incorrect release version.
tsg_app_version <- as.character(utils::packageVersion("TSGenerator"))
tsg_app_title <- paste("TSGenerator", tsg_app_version)

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "R", "mod_dashboard.R"), local = TRUE)
source(file.path(app_dir, "R", "mod_acquisition.R"), local = TRUE)
source(file.path(app_dir, "R", "mod_spatial.R"), local = TRUE)
source(file.path(app_dir, "R", "mod_quality.R"), local = TRUE)
source(file.path(app_dir, "R", "mod_temporal.R"), local = TRUE)
source(file.path(app_dir, "R", "mod_export.R"), local = TRUE)

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(title = tsg_app_title, titleWidth = 245),
  shinydashboard::dashboardSidebar(
    width = 245,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = shiny::icon("tachometer-alt"), selected = TRUE),
      shinydashboard::menuItem("Data Acquisition", tabName = "acquisition", icon = shiny::icon("cloud-download-alt")),
      shinydashboard::menuItem("Spatial Processing", tabName = "spatial", icon = shiny::icon("layer-group")),
      shinydashboard::menuItem("Quality", tabName = "quality", icon = shiny::icon("check-circle")),
      shinydashboard::menuItem("Time-Series Analysis", tabName = "temporal", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("Results & Export", tabName = "export", icon = shiny::icon("file-export")),
      shiny::tags$li(class = "header", "TSGENERATOR"),
      shinydashboard::menuItem("About", tabName = "about", icon = shiny::icon("info-circle"))
    )
  ),
  shinydashboard::dashboardBody(
    shiny::tags$head(shiny::includeCSS("www/tsgenerator.css")),
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "dashboard",
        shiny::tags$div(class = "tsg-brand",
          shiny::tags$img(src = "TSGenerator.png", alt = "TSGenerator logo"),
          shiny::tags$div(shiny::tags$h2(tsg_app_title), shiny::tags$p("Copernicus HR-VPP acquisition, processing and time-series analysis"))
        ),
        mod_dashboard_ui("dashboard")
      ),
      shinydashboard::tabItem(tabName = "acquisition",
        mod_acquisition_ui("acquisition")
      ),
      shinydashboard::tabItem(tabName = "spatial",
        mod_spatial_ui("spatial")
      ),
      shinydashboard::tabItem(tabName = "quality",
        mod_quality_ui("quality")
      ),
      shinydashboard::tabItem(tabName = "temporal",
        mod_temporal_ui("temporal")
      ),
      shinydashboard::tabItem(tabName = "export",
        mod_export_ui("export")
      ),
      shinydashboard::tabItem(tabName = "about",
        shinydashboard::box(width = 12, title = paste("About", tsg_app_title), status = "info", solidHeader = TRUE,
          shiny::p("The unified Shiny application is a graphical layer over the public TSGenerator 2.0 R API."),
          shiny::p("The GUI must not duplicate scientific algorithms: the same core functions are used by scripted and graphical workflows."),
          shiny::tags$code("library(TSGenerator); runTSapp()")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mod_dashboard_server("dashboard")
  acquisition_state <- mod_acquisition_server("acquisition")
  spatial_state <- mod_spatial_server("spatial")
  quality_state <- mod_quality_server("quality")
  temporal_state <- mod_temporal_server("temporal", spatial_result = spatial_state$extraction)
  mod_export_server(
    "export",
    acquisition_state = acquisition_state,
    spatial_state = spatial_state,
    quality_state = quality_state,
    temporal_state = temporal_state
  )
}

shiny::shinyApp(ui, server)
