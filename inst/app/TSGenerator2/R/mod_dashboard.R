mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("version_box"), width = 3),
      shinydashboard::valueBoxOutput(ns("wekeo_box"), width = 3),
      shinydashboard::valueBoxOutput(ns("geo_box"), width = 3),
      shinydashboard::valueBoxOutput(ns("temporal_box"), width = 3)
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, title = "TSGenerator 2.0 workflow", status = "primary",
        solidHeader = TRUE,
        shiny::tags$div(class = "tsg-workflow",
          workflow_step("1", "Acquire", "Search and download ST/VPP from WEkEO", "cloud-download-alt"),
          workflow_arrow(),
          workflow_step("2", "Prepare", "Load HR-VPP rasters and polygon AOIs", "layer-group"),
          workflow_arrow(),
          workflow_step("3", "Extract", "Extract ST and VPP information", "draw-polygon"),
          workflow_arrow(),
          workflow_step("4", "Quality", "Inspect QFLAG and temporal completeness", "check-circle"),
          workflow_arrow(),
          workflow_step("5", "Analyze", "Missingness, optional imputation and GAM", "chart-line"),
          workflow_arrow(),
          workflow_step("6", "Export", "Save results and reproducible R code", "file-export")
        )
      ),
      shinydashboard::box(
        width = 4, title = "Environment", status = "info", solidHeader = TRUE,
        shiny::tableOutput(ns("environment")),
        shiny::actionButton(ns("refresh"), "Refresh", icon = shiny::icon("sync"), class = "btn-primary"),
        shiny::tags$hr(),
        shiny::tags$small("The dashboard only inspects the installed TSGenerator core. It does not download data.")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "2.0 core availability", status = "success", solidHeader = TRUE,
        DT::DTOutput(ns("api_table"))
      )
    )
  )
}

workflow_step <- function(number, title, text, icon) {
  shiny::tags$div(class = "tsg-step",
    shiny::tags$div(class = "tsg-step-number", number),
    shiny::icon(icon),
    shiny::tags$strong(title),
    shiny::tags$span(text)
  )
}

workflow_arrow <- function() shiny::tags$div(class = "tsg-arrow", shiny::icon("chevron-right"))

mod_dashboard_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$refresh, refresh(refresh() + 1L))

    api_groups <- list(
      Acquisition = c("download_st", "download_vpp", "check_wekeo", "check_wekeo_integration"),
      Geospatial = c("geospatial_plan", "extract_ts", "extract_vpp"),
      `Product quality` = c("quality_info", "classify_quality", "summarize_quality", "mask_quality"),
      `Time series` = c("temporal_plan", "summarize_missingness", "assess_ts_quality", "impute_ts", "model_missingness", "plot_missingness")
    )

    status_data <- shiny::reactive({
      refresh()
      exports <- getNamespaceExports("TSGenerator")
      do.call(rbind, lapply(names(api_groups), function(group) {
        funs <- api_groups[[group]]
        data.frame(
          Module = group,
          Function = paste0(funs, "()"),
          Available = vapply(funs, function(x) {
            x %in% exports && exists(x, envir = asNamespace("TSGenerator"), inherits = FALSE)
          }, logical(1)),
          stringsAsFactors = FALSE
        )
      }))
    })

    output$version_box <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(as.character(utils::packageVersion("TSGenerator")), "TSGenerator", icon = shiny::icon("cube"), color = "aqua")
    })
    output$wekeo_box <- shinydashboard::renderValueBox({
      ok <- all(status_data()$Available[status_data()$Module == "Acquisition"])
      shinydashboard::valueBox(if (ok) "Ready" else "Check", "Acquisition core", icon = shiny::icon("cloud"), color = if (ok) "green" else "yellow")
    })
    output$geo_box <- shinydashboard::renderValueBox({
      ok <- all(status_data()$Available[status_data()$Module == "Geospatial"])
      shinydashboard::valueBox(if (ok) "Ready" else "Check", "Geospatial core", icon = shiny::icon("map"), color = if (ok) "green" else "yellow")
    })
    output$temporal_box <- shinydashboard::renderValueBox({
      ok <- all(status_data()$Available[status_data()$Module == "Time series"])
      shinydashboard::valueBox(if (ok) "Ready" else "Check", "Temporal core", icon = shiny::icon("chart-line"), color = if (ok) "green" else "yellow")
    })

    output$environment <- shiny::renderTable({
      refresh()
      data.frame(
        Component = c("R", "TSGenerator", "terra", "hdar", "Credentials"),
        Status = c(
          paste(R.version$major, R.version$minor, sep = "."),
          as.character(utils::packageVersion("TSGenerator")),
          if (requireNamespace("terra", quietly = TRUE)) as.character(utils::packageVersion("terra")) else "Not installed",
          if (requireNamespace("hdar", quietly = TRUE)) as.character(utils::packageVersion("hdar")) else "Not installed",
          if (file.exists(path.expand("~/.hdarc"))) "~/.hdarc found" else "Not configured"
        ), check.names = FALSE
      )
    }, striped = TRUE, bordered = FALSE, spacing = "xs")

    output$api_table <- DT::renderDT({
      d <- status_data()
      d$Status <- ifelse(d$Available, "Ready", "Unavailable")
      d$Available <- NULL
      DT::datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 20), class = "compact stripe")
    })
  })
}
