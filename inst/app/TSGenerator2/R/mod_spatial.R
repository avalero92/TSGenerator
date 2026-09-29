# Spatial Processing & Extraction module --------------------------------
# Thin GUI layer over geospatial_plan(), extract_ts() and extract_vpp().

mod_spatial_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("raster_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("aoi_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("plan_status"), width = 4)
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "1. Raster products", status = "primary", solidHeader = TRUE,
        shiny::radioButtons(ns("raster_source"), "Input source",
          choices = c("Existing files / directory" = "path", "Upload TIFF files" = "upload"), selected = "path"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'path'", ns("raster_source")),
          shiny::textInput(ns("raster_path"), "TIFF file or directory", placeholder = "C:/.../HR-VPP")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("raster_source")),
          shiny::fileInput(ns("raster_upload"), "Upload GeoTIFF(s)", multiple = TRUE, accept = c(".tif", ".tiff"))
        ),
        shiny::selectInput(ns("product_family"), "Extraction workflow",
          choices = c("Auto-detect from filenames" = "AUTO", "Seasonal Trajectories (ST)" = "ST", "VPP parameters" = "VPP"), selected = "AUTO"
        ),
        shiny::actionButton(ns("inspect_rasters"), "Inspect rasters", icon = shiny::icon("layer-group"), class = "btn-info btn-block")
      ),
      shinydashboard::box(
        width = 4, title = "2. Area of interest", status = "primary", solidHeader = TRUE,
        shiny::radioButtons(ns("aoi_source"), "AOI source",
          choices = c("Existing vector file" = "path", "Upload vector" = "upload"), selected = "path"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'path'", ns("aoi_source")),
          shiny::textInput(ns("aoi_path"), "Polygon file", placeholder = "C:/.../parcels.gpkg or parcels.shp")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("aoi_source")),
          shiny::fileInput(ns("aoi_upload"), "Upload AOI",
            multiple = TRUE, accept = c(".gpkg", ".geojson", ".json", ".shp", ".dbf", ".shx", ".prj", ".cpg")
          ),
          shiny::tags$p(class = "help-block", "For a Shapefile, select all companion files (.shp, .dbf, .shx, .prj, etc.) together.")
        ),
        shiny::actionButton(ns("inspect_aoi"), "Inspect AOI", icon = shiny::icon("draw-polygon"), class = "btn-info btn-block"),
        shiny::hr(),
        shiny::uiOutput(ns("id_col_ui")),
        shiny::checkboxInput(ns("transform"), "Transform AOI to raster CRS when needed", TRUE)
      ),
      shinydashboard::box(
        width = 4, title = "3. Extraction settings", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("summary_fun"), "Polygon statistic", choices = c("Median" = "median", "Mean" = "mean", "Minimum" = "min", "Maximum" = "max", "Sum" = "sum"), selected = "median"),
        shiny::checkboxInput(ns("exact"), "Exact polygon-cell fractions", FALSE),
        shiny::checkboxInput(ns("touches"), "Include boundary-touched cells", FALSE),
        shiny::checkboxInput(ns("na_rm"), "Remove NA pixels", TRUE),
        shiny::actionButton(ns("make_plan"), "Validate spatial plan", icon = shiny::icon("clipboard-check"), class = "btn-primary btn-block"),
        shiny::actionButton(ns("extract"), "Run extraction", icon = shiny::icon("play"), class = "btn-success btn-block")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 6, title = "Input inspection", status = "info", solidHeader = TRUE,
        DT::DTOutput(ns("inspection")),
        shiny::verbatimTextOutput(ns("plan_text"))
      ),
      shinydashboard::box(
        width = 6, title = "Extraction preview", status = "info", solidHeader = TRUE,
        shinycssloaders::withSpinner(DT::DTOutput(ns("extraction_table")), type = 6),
        shiny::uiOutput(ns("extraction_note"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, title = "Reproducible R code", status = "info", solidHeader = TRUE,
        shiny::verbatimTextOutput(ns("rcode"))
      ),
      shinydashboard::box(
        width = 4, title = "Export", status = "info", solidHeader = TRUE,
        shiny::downloadButton(ns("download_csv"), "Download extracted CSV", class = "btn-success btn-block"),
        shiny::downloadButton(ns("download_code"), "Save R script", class = "btn-default btn-block"),
        shiny::tags$p(class = "help-block", "The GUI calls the public TSGenerator geospatial API; no extraction algorithm is duplicated here.")
      )
    )
  )
}

mod_spatial_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    raster_info <- shiny::reactiveVal(NULL)
    aoi_info <- shiny::reactiveVal(NULL)
    plan_obj <- shiny::reactiveVal(NULL)
    extraction <- shiny::reactiveVal(NULL)
    upload_dirs <- shiny::reactiveValues(aoi = NULL, raster = NULL)

    clean_text <- function(x) if (is.null(x) || !nzchar(trimws(x))) NULL else trimws(x)

    raster_input <- shiny::reactive({
      if (identical(input$raster_source, "upload")) {
        shiny::req(input$raster_upload)
        # Preserve original HR-VPP filenames: ST date parsing and VPP metadata
        # inference depend on the standard CLMS file names.
        td <- tempfile("tsg_raster_"); dir.create(td)
        out <- file.path(td, input$raster_upload$name)
        ok <- file.copy(input$raster_upload$datapath, out, overwrite = TRUE)
        shiny::validate(shiny::need(all(ok), "Could not stage uploaded GeoTIFF files."))
        upload_dirs$raster <- td
        return(out)
      }
      p <- clean_text(input$raster_path)
      shiny::req(p)
      p
    })

    aoi_input <- shiny::reactive({
      if (identical(input$aoi_source, "path")) {
        p <- clean_text(input$aoi_path); shiny::req(p); return(p)
      }
      shiny::req(input$aoi_upload)
      # fileInput renames temporary files; reconstruct original names so GDAL can
      # resolve Shapefile sidecars reliably.
      td <- tempfile("tsg_aoi_"); dir.create(td)
      for (i in seq_len(nrow(input$aoi_upload))) {
        file.copy(input$aoi_upload$datapath[i], file.path(td, input$aoi_upload$name[i]), overwrite = TRUE)
      }
      upload_dirs$aoi <- td
      nm <- input$aoi_upload$name
      primary <- which(tolower(tools::file_ext(nm)) %in% c("gpkg", "geojson", "json", "shp"))[1]
      shiny::validate(shiny::need(!is.na(primary), "Upload a .gpkg, .geojson or complete Shapefile."))
      file.path(td, nm[primary])
    })

    detect_family <- function(files) {
      nm <- toupper(basename(files))
      if (any(grepl("^VPP_|_(SOSD|EOSD|MAXD|LENGTH|SOSV|EOSV|MINV|MAXV|AMPL|LSLOPE|RSLOPE|SPROD|TPROD)(\\.TIF|\\.TIFF)?$", nm))) "VPP" else "ST"
    }

    resolved_family <- shiny::reactive({
      if (!identical(input$product_family, "AUTO")) return(input$product_family)
      ri <- raster_info()
      if (!is.null(ri)) return(ri$family)
      x <- raster_input()
      files <- if (length(x) == 1L && dir.exists(x)) list.files(x, "\\.(tif|tiff)$", full.names = TRUE, ignore.case = TRUE) else x
      detect_family(files)
    })

    inspect_rasters <- function() {
      x <- raster_input()
      rr <- terra::rast(if (length(x) == 1L && dir.exists(x)) list.files(x, "\\.(tif|tiff)$", full.names = TRUE, ignore.case = TRUE) else x)
      src <- terra::sources(rr)
      fam <- if (identical(input$product_family, "AUTO")) detect_family(src) else input$product_family
      e <- terra::ext(rr); rs <- terra::res(rr)
      list(object = rr, sources = src, family = fam,
           table = data.frame(Property = c("Workflow", "Layers", "Rows x columns", "Resolution", "CRS", "Extent"),
             Value = c(fam, terra::nlyr(rr), paste(terra::nrow(rr), "x", terra::ncol(rr)), paste(signif(rs, 6), collapse = " x "),
               terra::crs(rr, proj = TRUE), paste(signif(c(e$xmin,e$xmax,e$ymin,e$ymax), 7), collapse = ", ")), stringsAsFactors = FALSE))
    }

    inspect_aoi <- function() {
      p <- aoi_input(); v <- terra::vect(p)
      gt <- unique(terra::geomtype(v)); attrs <- names(v); e <- terra::ext(v)
      list(object = v, path = p, attrs = attrs,
           table = data.frame(Property = c("Features", "Geometry", "Attributes", "CRS", "Extent"),
             Value = c(nrow(v), paste(gt, collapse = ", "), if (length(attrs)) paste(attrs, collapse = ", ") else "(none)",
               terra::crs(v, proj = TRUE), paste(signif(c(e$xmin,e$xmax,e$ymin,e$ymax), 7), collapse = ", ")), stringsAsFactors = FALSE))
    }

    shiny::observeEvent(input$inspect_rasters, {
      z <- tryCatch(inspect_rasters(), error = function(e) e)
      if (inherits(z, "error")) shiny::showNotification(conditionMessage(z), type = "error", duration = NULL)
      else { raster_info(z); plan_obj(NULL); extraction(NULL); shiny::showNotification("Raster products inspected.", type = "message") }
    })
    shiny::observeEvent(input$inspect_aoi, {
      z <- tryCatch(inspect_aoi(), error = function(e) e)
      if (inherits(z, "error")) shiny::showNotification(conditionMessage(z), type = "error", duration = NULL)
      else { aoi_info(z); plan_obj(NULL); extraction(NULL); shiny::showNotification("AOI inspected.", type = "message") }
    })

    output$id_col_ui <- shiny::renderUI({
      ai <- aoi_info(); choices <- c("Generated feature ID" = "")
      if (!is.null(ai) && length(ai$attrs)) choices <- c(choices, stats::setNames(ai$attrs, ai$attrs))
      shiny::selectInput(session$ns("id_col"), "Feature ID field", choices = choices, selected = "")
    })
    current_id <- shiny::reactive({ z <- input$id_col; if (is.null(z) || !nzchar(z)) NULL else z })

    build_plan <- function() {
      ai <- aoi_info(); if (is.null(ai)) ai <- inspect_aoi()
      TSGenerator::geospatial_plan(raster_input(), ai$object, id_col = current_id(), transform = isTRUE(input$transform))
    }
    shiny::observeEvent(input$make_plan, {
      z <- tryCatch(build_plan(), error = function(e) e)
      if (inherits(z, "error")) { plan_obj(NULL); shiny::showNotification(conditionMessage(z), type = "error", duration = NULL) }
      else { plan_obj(z); shiny::showNotification("Spatial plan is valid and ready for extraction.", type = "message") }
    })

    shiny::observeEvent(input$extract, {
      ai <- aoi_info(); if (is.null(ai)) ai <- tryCatch(inspect_aoi(), error = function(e) e)
      if (inherits(ai, "error")) { shiny::showNotification(conditionMessage(ai), type = "error", duration = NULL); return() }
      fam <- resolved_family()
      args <- list(x = raster_input(), polygons = ai$object, id_col = current_id(), fun = input$summary_fun,
                   transform = isTRUE(input$transform), na.rm = isTRUE(input$na_rm), exact = isTRUE(input$exact), touches = isTRUE(input$touches))
      z <- tryCatch(if (identical(fam, "VPP")) do.call(TSGenerator::extract_vpp, args) else do.call(TSGenerator::extract_ts, args), error = function(e) e)
      if (inherits(z, "error")) shiny::showNotification(conditionMessage(z), type = "error", duration = NULL)
      else { extraction(z); if (is.null(plan_obj())) plan_obj(tryCatch(build_plan(), error = function(e) NULL)); shiny::showNotification(sprintf("Extraction complete: %d records.", nrow(z)), type = "message") }
    })

    output$raster_status <- shinydashboard::renderValueBox({
      z <- raster_info(); shinydashboard::valueBox(if (is.null(z)) "Not inspected" else paste(z$family, terra::nlyr(z$object), "layer(s)"), "Raster input", icon = shiny::icon("layer-group"), color = if (is.null(z)) "yellow" else "aqua")
    })
    output$aoi_status <- shinydashboard::renderValueBox({
      z <- aoi_info(); shinydashboard::valueBox(if (is.null(z)) "Not inspected" else paste(nrow(z$object), "polygon(s)"), "Area of interest", icon = shiny::icon("draw-polygon"), color = if (is.null(z)) "yellow" else "aqua")
    })
    output$plan_status <- shinydashboard::renderValueBox({
      z <- plan_obj(); shinydashboard::valueBox(if (is.null(z)) "Pending" else "Validated", "Spatial plan", icon = shiny::icon(if (is.null(z)) "clipboard" else "check"), color = if (is.null(z)) "yellow" else "green")
    })

    output$inspection <- DT::renderDT({
      ri <- raster_info(); ai <- aoi_info()
      if (is.null(ri) && is.null(ai)) return(DT::datatable(data.frame(Status = "Inspect raster products and AOI to view metadata."), options = list(dom = "t"), rownames = FALSE))
      tabs <- rbind(if (!is.null(ri)) data.frame(Input = "Raster", ri$table) else NULL,
                    if (!is.null(ai)) data.frame(Input = "AOI", ai$table) else NULL)
      DT::datatable(tabs, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    output$plan_text <- shiny::renderPrint({
      p <- plan_obj(); if (is.null(p)) cat("Validate the spatial plan before extraction to check CRS and geometry compatibility.\n") else print(p)
    })
    output$extraction_table <- DT::renderDT({
      z <- extraction(); if (is.null(z)) return(DT::datatable(data.frame(Status = "No extraction has been run."), options = list(dom = "t"), rownames = FALSE))
      DT::datatable(utils::head(as.data.frame(z), 500), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
    })
    output$extraction_note <- shiny::renderUI({
      z <- extraction(); if (is.null(z)) return(NULL)
      shiny::tags$div(class = "alert alert-success", shiny::strong("Extraction complete. "), sprintf("%d records; %d feature(s).", nrow(z), length(unique(z$ID))))
    })

    make_code <- shiny::reactive({
      x <- if (identical(input$raster_source, "path")) clean_text(input$raster_path) else "<uploaded GeoTIFF paths>"
      a <- if (identical(input$aoi_source, "path")) clean_text(input$aoi_path) else "<uploaded AOI path>"
      fam <- tryCatch(resolved_family(), error = function(e) if (identical(input$product_family,"VPP")) "VPP" else "ST")
      fun <- if (identical(fam, "VPP")) "extract_vpp" else "extract_ts"
      id <- current_id(); q <- function(z) paste0('"', gsub('\\\\','/',z), '"')
      codelines <- c("library(TSGenerator)", "library(terra)", "",
        paste0("rasters <- ", if (identical(input$raster_source,"path") && !is.null(x)) q(x) else 'c("path/to/product_1.tif", "path/to/product_2.tif")'),
        paste0("aoi <- terra::vect(", if (identical(input$aoi_source,"path") && !is.null(a)) q(a) else '"path/to/aoi.gpkg"', ")"), "",
        paste0("result <- ", fun, "("), "  x = rasters,", "  polygons = aoi,",
        if (!is.null(id)) paste0("  id_col = ", q(id), ",") else NULL,
        paste0("  fun = ", q(input$summary_fun), ","), paste0("  transform = ", toupper(as.character(isTRUE(input$transform))), ","),
        paste0("  na.rm = ", toupper(as.character(isTRUE(input$na_rm))), ","), paste0("  exact = ", toupper(as.character(isTRUE(input$exact))), ","),
        paste0("  touches = ", toupper(as.character(isTRUE(input$touches))), ")"))
      paste(codelines, collapse = "\n")
    })
    output$rcode <- shiny::renderText(make_code())
    output$download_csv <- shiny::downloadHandler(filename = function() paste0("TSGenerator_", tolower(resolved_family()), "_extraction.csv"), content = function(file) { shiny::req(extraction()); utils::write.csv(as.data.frame(extraction()), file, row.names = FALSE) })
    output$download_code <- shiny::downloadHandler(filename = function() "TSGenerator_spatial_extraction.R", content = function(file) writeLines(make_code(), file))
    # Expose the validated extraction to downstream modules without duplicating
    # the geospatial workflow.
    list(extraction = shiny::reactive(extraction()), family = shiny::reactive(resolved_family()), code = shiny::reactive(make_code()))
  })
}
