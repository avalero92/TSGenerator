# Unified WEkEO acquisition module ---------------------------------------
# This module is intentionally a thin GUI layer over the public TSGenerator API.

mod_acquisition_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("wekeo_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("product_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("result_status"), width = 4)
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "1. WEkEO & product", status = "primary", solidHeader = TRUE,
        shiny::actionButton(ns("check_wekeo"), "Check WEkEO", icon = shiny::icon("plug"), class = "btn-info"),
        shiny::hr(),
        shiny::radioButtons(ns("product_family"), "HR-VPP product", choices = c("Seasonal Trajectories (ST)" = "ST", "Phenology & Productivity (VPP)" = "VPP"), selected = "ST"),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'ST'", ns("product_family")),
          shiny::checkboxGroupInput(ns("st_product"), "ST layer", choices = c("PPI", "QFLAG"), selected = "PPI")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'VPP'", ns("product_family")),
          shiny::selectizeInput(ns("vpp_product"), "VPP parameter(s)",
            choices = c("MINV", "MAXD", "LENGTH", "SOSD", "QFLAG", "EOSV", "TPROD", "MAXV", "AMPL", "SOSV", "LSLOPE", "EOSD", "RSLOPE", "SPROD"),
            selected = c("SOSD", "MAXD", "EOSD", "LENGTH"), multiple = TRUE
          ),
          shiny::checkboxGroupInput(ns("vpp_season"), "Season", choices = c("Season 1" = "s1", "Season 2" = "s2"), selected = "s1")
        )
      ),
      shinydashboard::box(
        width = 4, title = "2. Period & spatial filter", status = "primary", solidHeader = TRUE,
        shiny::dateRangeInput(ns("dates"), "Period", start = as.Date("2020-04-01"), end = as.Date("2020-04-30"), format = "yyyy-mm-dd"),
        shiny::textInput(ns("tile_id"), "Sentinel-2 tile (optional)", placeholder = "e.g. 30TXM"),
        shiny::checkboxInput(ns("use_bbox"), "Use bounding box (EPSG:4326)", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("use_bbox")),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("xmin"), "xmin", value = -1, step = 0.01)),
            shiny::column(6, shiny::numericInput(ns("ymin"), "ymin", value = 40, step = 0.01)),
            shiny::column(6, shiny::numericInput(ns("xmax"), "xmax", value = 0, step = 0.01)),
            shiny::column(6, shiny::numericInput(ns("ymax"), "ymax", value = 41, step = 0.01))
          )
        ),
        shiny::textInput(ns("product_version"), "Product version (optional)", placeholder = "leave blank for current/default")
      ),
      shinydashboard::box(
        width = 4, title = "3. Search & download", status = "primary", solidHeader = TRUE,
        shiny::numericInput(ns("limit"), "Maximum search results (optional)", value = NA, min = 1, step = 1),
        shiny::actionButton(ns("search"), "Search / Preview", icon = shiny::icon("search"), class = "btn-primary btn-block"),
        shiny::hr(),
        shiny::textInput(ns("output_dir"), "Download directory", value = file.path(path.expand("~"), "TSGenerator_downloads")),
        shiny::checkboxInput(ns("overwrite"), "Overwrite existing products", FALSE),
        shiny::actionButton(ns("download"), "Download previewed query", icon = shiny::icon("download"), class = "btn-success btn-block"),
        shiny::tags$p(class = "help-block", "Download repeats the validated query using the public download_st()/download_vpp() API. No acquisition algorithm is duplicated in Shiny.")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, title = "WEkEO results", status = "info", solidHeader = TRUE,
        shinycssloaders::withSpinner(DT::DTOutput(ns("results")), type = 6),
        shiny::uiOutput(ns("result_note"))
      ),
      shinydashboard::box(
        width = 4, title = "Reproducible R code", status = "info", solidHeader = TRUE,
        shiny::tags$p("The GUI action can be reproduced with the public TSGenerator API:"),
        shiny::verbatimTextOutput(ns("rcode")),
        shiny::downloadButton(ns("download_code"), "Save R script", class = "btn-default")
      )
    )
  )
}

mod_acquisition_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    preview <- shiny::reactiveVal(NULL)
    preview_args <- shiny::reactiveVal(NULL)
    last_download <- shiny::reactiveVal(NULL)
    diagnostics <- shiny::reactiveVal(NULL)
    busy <- shiny::reactiveVal(FALSE)

    clean_text <- function(x) if (is.null(x) || !nzchar(trimws(x))) NULL else trimws(x)
    current_bbox <- shiny::reactive({
      if (!isTRUE(input$use_bbox)) return(NULL)
      z <- c(input$xmin, input$ymin, input$xmax, input$ymax)
      shiny::validate(shiny::need(all(is.finite(z)), "Bounding-box values must be finite."), shiny::need(z[1] < z[3] && z[2] < z[4], "Bounding box must satisfy xmin < xmax and ymin < ymax."))
      z
    })
    current_limit <- shiny::reactive({
      z <- suppressWarnings(as.numeric(input$limit))
      if (!length(z) || is.na(z) || z < 1) NULL else as.integer(z)
    })

    request_args <- shiny::reactive({
      shiny::req(input$dates, length(input$dates) == 2L)
      common <- list(
        start = as.character(input$dates[1]), end = as.character(input$dates[2]),
        tile_id = clean_text(input$tile_id), bbox = current_bbox(),
        product_version = clean_text(input$product_version), limit = current_limit()
      )
      if (identical(input$product_family, "ST")) {
        shiny::validate(shiny::need(length(input$st_product) > 0L, "Select at least one ST layer."))
        c(common, list(product = input$st_product))
      } else {
        shiny::validate(shiny::need(length(input$vpp_product) > 0L, "Select at least one VPP parameter."), shiny::need(length(input$vpp_season) > 0L, "Select at least one VPP season."))
        c(common, list(product = input$vpp_product, season = input$vpp_season))
      }
    })

    call_api <- function(download = FALSE, args = request_args()) {
      args$download <- download
      args$prompt <- FALSE
      args$quiet <- TRUE
      if (download) {
        out <- clean_text(input$output_dir)
        if (is.null(out)) stop("Choose a download directory.", call. = FALSE)
        args$output_dir <- out
        args$overwrite <- isTRUE(input$overwrite)
      }
      if (identical(input$product_family, "ST")) {
        do.call(TSGenerator::download_st, args)
      } else {
        do.call(TSGenerator::download_vpp, args)
      }
    }

    shiny::observeEvent(input$check_wekeo, {
      busy(TRUE); on.exit(busy(FALSE), add = TRUE)
      d <- tryCatch(TSGenerator::check_wekeo(online = TRUE), error = function(e) e)
      if (inherits(d, "error")) {
        diagnostics(NULL)
        shiny::showNotification(conditionMessage(d), type = "error", duration = NULL)
      } else {
        diagnostics(d)
        ok <- all(d$ok[d$check %in% c("authentication", "schema_ST", "schema_VPP")])
        shiny::showNotification(if (ok) "WEkEO authentication and ST/VPP schemas are available." else "WEkEO diagnostics completed with issues.", type = if (ok) "message" else "warning")
      }
    })

    shiny::observeEvent(input$search, {
      busy(TRUE); on.exit(busy(FALSE), add = TRUE)
      z <- tryCatch(call_api(FALSE), error = function(e) e)
      if (inherits(z, "error")) {
        shiny::showNotification(conditionMessage(z), type = "error", duration = NULL)
      } else {
        preview(z); preview_args(request_args()); last_download(NULL)
        shiny::showNotification(sprintf("Preview complete: %s result(s).", sum(z$summary$n_results, na.rm = TRUE)), type = "message")
      }
    })

    shiny::observeEvent(input$download, {
      shiny::req(preview())
      busy(TRUE); on.exit(busy(FALSE), add = TRUE)
      z <- tryCatch(call_api(TRUE, args = preview_args()), error = function(e) e)
      if (inherits(z, "error")) {
        shiny::showNotification(conditionMessage(z), type = "error", duration = NULL)
      } else {
        last_download(z)
        shiny::showNotification(paste0("Download operation completed. Output: ", z$output_dir), type = "message", duration = 8)
      }
    })

    output$wekeo_status <- shinydashboard::renderValueBox({
      d <- diagnostics()
      if (is.null(d)) return(shinydashboard::valueBox("Not checked", "WEkEO", icon = shiny::icon("cloud"), color = "yellow"))
      auth <- d$ok[d$check == "authentication"]
      good <- length(auth) == 1L && isTRUE(auth)
      shinydashboard::valueBox(if (good) "Connected" else "Issue", "WEkEO", icon = shiny::icon(if (good) "check" else "exclamation-triangle"), color = if (good) "green" else "red")
    })
    output$product_status <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(input$product_family %||% "ST", "Selected product", icon = shiny::icon("leaf"), color = "aqua")
    })
    output$result_status <- shinydashboard::renderValueBox({
      z <- preview(); n <- if (is.null(z)) 0L else sum(z$summary$n_results, na.rm = TRUE)
      shinydashboard::valueBox(n, "Preview results", icon = shiny::icon("search"), color = if (n > 0) "green" else "yellow")
    })

    output$results <- DT::renderDT({
      z <- preview()
      if (is.null(z)) return(DT::datatable(data.frame(Status = "Run Search / Preview to query WEkEO."), options = list(dom = "t"), rownames = FALSE))
      DT::datatable(z$summary, rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
    })
    output$result_note <- shiny::renderUI({
      z <- last_download()
      if (is.null(z)) return(NULL)
      shiny::tags$div(class = "alert alert-success", shiny::strong("Last download: "), z$output_dir)
    })

    make_code <- shiny::reactive({
      a <- request_args()
      fun <- if (identical(input$product_family, "ST")) "download_st" else "download_vpp"
      q <- function(x) paste0('"', x, '"')
      vec <- function(x) if (length(x) == 1L) q(x) else paste0("c(", paste(q(x), collapse = ", "), ")")
      lines <- c(
        "library(TSGenerator)", "",
        paste0("x <- ", fun, "("),
        paste0("  start = ", q(a$start), ","), paste0("  end = ", q(a$end), ","),
        paste0("  product = ", vec(a$product), ",")
      )
      if (!is.null(a$season)) lines <- c(lines, paste0("  season = ", vec(a$season), ","))
      if (!is.null(a$tile_id)) lines <- c(lines, paste0("  tile_id = ", q(a$tile_id), ","))
      if (!is.null(a$bbox)) lines <- c(lines, paste0("  bbox = c(", paste(a$bbox, collapse = ", "), "),"))
      if (!is.null(a$product_version)) lines <- c(lines, paste0("  product_version = ", q(a$product_version), ","))
      if (!is.null(a$limit)) lines <- c(lines, paste0("  limit = ", a$limit, ","))
      lines <- c(lines, "  download = FALSE", ")")
      paste(lines, collapse = "\n")
    })
    output$rcode <- shiny::renderText(make_code())
    output$download_code <- shiny::downloadHandler(
      filename = function() paste0("TSGenerator_", tolower(input$product_family), "_query.R"),
      content = function(file) writeLines(make_code(), file)
    )

    # Expose session state to the integration/export layer. This does not
    # duplicate acquisition logic; downstream modules receive read-only reactives.
    list(
      preview = shiny::reactive(preview()),
      download = shiny::reactive(last_download()),
      diagnostics = shiny::reactive(diagnostics()),
      product = shiny::reactive(input$product_family),
      code = shiny::reactive(make_code())
    )
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
