mod_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("product_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("qflag_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("mask_status"), width = 4)
    ),
    shiny::fluidRow(
      shinydashboard::box(width = 4, title = "1. Product quality", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("type"), "HR-VPP family", c("Seasonal Trajectories (ST)"="ST", "Vegetation Phenology & Productivity (VPP)"="VPP")),
        shiny::actionButton(ns("definitions"), "Load quality definitions", icon=shiny::icon("list"), class="btn-primary btn-block"),
        shiny::tags$p(class="help-block", "ST and VPP QFLAG semantics are kept separate from temporal completeness.")),
      shinydashboard::box(width = 4, title = "2. QFLAG input", status = "primary", solidHeader = TRUE,
        shiny::radioButtons(ns("q_source"), "Source", c("File / directory path"="path", "Upload GeoTIFF(s)"="upload"), inline=TRUE),
        shiny::conditionalPanel(sprintf("input['%s'] == 'path'", ns("q_source")), shiny::textInput(ns("q_path"), "QFLAG path or directory", placeholder="C:/.../QFLAG.tif")),
        shiny::conditionalPanel(sprintf("input['%s'] == 'upload'", ns("q_source")), shiny::fileInput(ns("q_upload"), "QFLAG GeoTIFF(s)", multiple=TRUE, accept=c(".tif",".tiff"))),
        shiny::actionButton(ns("summarize"), "Summarize QFLAG", icon=shiny::icon("chart-bar"), class="btn-success btn-block")),
      shinydashboard::box(width = 4, title = "3. Optional quality mask", status = "primary", solidHeader = TRUE,
        shiny::textInput(ns("target_path"), "Target raster path", placeholder="Matching ST/VPP raster"),
        shiny::numericInput(ns("min_quality"), "Minimum QFLAG code (blank = TSGenerator default)", value=NA, min=0, max=10),
        shiny::actionButton(ns("mask"), "Apply quality mask", icon=shiny::icon("filter"), class="btn-warning btn-block"),
        shiny::downloadButton(ns("download_mask"), "Download masked GeoTIFF", class="btn-default btn-block"))
    ),
    shiny::fluidRow(
      shinydashboard::box(width=6, title="Quality definitions", status="info", solidHeader=TRUE, DT::DTOutput(ns("definitions_table"))),
      shinydashboard::box(width=6, title="QFLAG summary", status="info", solidHeader=TRUE, shinycssloaders::withSpinner(DT::DTOutput(ns("summary_table")), type=6))
    ),
    shiny::fluidRow(
      shinydashboard::box(width=8, title="Reproducible R code", status="info", solidHeader=TRUE, shiny::verbatimTextOutput(ns("rcode"))),
      shinydashboard::box(width=4, title="Scientific guardrails", status="info", solidHeader=TRUE,
        shiny::tags$ul(shiny::tags$li("QFLAG is categorical and is never resampled automatically."), shiny::tags$li("Target and QFLAG geometry must match."), shiny::tags$li("Default acceptance: ST 3-5; VPP 7-10.")))
    )
  )
}

mod_quality_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    defs <- shiny::reactiveVal(TSGenerator::quality_info("ST")); summ <- shiny::reactiveVal(NULL); masked <- shiny::reactiveVal(NULL); q_staged <- shiny::reactiveVal(NULL)
    q_input <- shiny::reactive({
      if (identical(input$q_source,"upload")) {
        shiny::req(input$q_upload); td <- tempfile("tsg_qflag_"); dir.create(td); p <- file.path(td,input$q_upload$name); file.copy(input$q_upload$datapath,p,overwrite=TRUE); q_staged(p); p
      } else { shiny::req(nzchar(trimws(input$q_path))); trimws(input$q_path) }
    })
    q_raster <- function() { p <- q_input(); files <- if(length(p)==1 && dir.exists(p)) list.files(p,"\\.(tif|tiff)$",full.names=TRUE,ignore.case=TRUE) else p; terra::rast(files) }
    shiny::observeEvent(input$definitions, { defs(TSGenerator::quality_info(input$type)) })
    shiny::observeEvent(input$summarize, {
      z <- tryCatch(TSGenerator::summarize_quality(q_raster(), type=input$type), error=function(e)e)
      if(inherits(z,"error")) shiny::showNotification(conditionMessage(z),type="error",duration=NULL) else { summ(z); shiny::showNotification("QFLAG summary complete.",type="message") }
    })
    shiny::observeEvent(input$mask, {
      z <- tryCatch({ shiny::req(nzchar(trimws(input$target_path))); x <- terra::rast(trimws(input$target_path)); q <- q_raster(); mq <- if(is.na(input$min_quality)) NULL else input$min_quality; TSGenerator::mask_quality(x,q,type=input$type,min_quality=mq) }, error=function(e)e)
      if(inherits(z,"error")) shiny::showNotification(conditionMessage(z),type="error",duration=NULL) else { masked(z); shiny::showNotification("Quality mask applied.",type="message") }
    })
    output$definitions_table <- DT::renderDT(DT::datatable(defs(), options=list(pageLength=11,scrollX=TRUE), rownames=FALSE, filter="top"))
    output$summary_table <- DT::renderDT({ z<-summ(); if(is.null(z)) return(DT::datatable(data.frame(Status="Summarize a QFLAG raster to view results."),options=list(dom="t"),rownames=FALSE)); DT::datatable(z,options=list(pageLength=11),rownames=FALSE,filter="top") })
    output$product_status <- shinydashboard::renderValueBox(shinydashboard::valueBox(input$type,"Product family",icon=shiny::icon("leaf"),color="aqua"))
    output$qflag_status <- shinydashboard::renderValueBox({z<-summ(); shinydashboard::valueBox(if(is.null(z)) "Pending" else paste(sum(z$Freq),"values"),"QFLAG summary",icon=shiny::icon("check-circle"),color=if(is.null(z))"yellow" else "green")})
    output$mask_status <- shinydashboard::renderValueBox({z<-masked(); shinydashboard::valueBox(if(is.null(z)) "Not applied" else "Ready","Quality mask",icon=shiny::icon("filter"),color=if(is.null(z))"yellow" else "green")})
    output$rcode <- shiny::renderText({ mq <- if(is.na(input$min_quality)) "NULL" else as.character(input$min_quality); paste0("library(TSGenerator)\nlibrary(terra)\n\nqflag <- terra::rast(\"", if(identical(input$q_source,"path")) input$q_path else "path/to/QFLAG.tif", "\")\nsummary <- summarize_quality(qflag, type = \"",input$type,"\")\n\n# Optional masking\ntarget <- terra::rast(\"",input$target_path,"\")\nmasked <- mask_quality(target, qflag, type = \"",input$type,"\", min_quality = ",mq,")") })
    output$download_mask <- shiny::downloadHandler(filename=function() paste0("TSGenerator_",input$type,"_quality_mask.tif"), content=function(file){ shiny::req(masked()); terra::writeRaster(masked(),file,overwrite=TRUE) })
    list(summary=shiny::reactive(summ()), masked=shiny::reactive(masked()), type=shiny::reactive(input$type), code=shiny::reactive({ mq <- if(is.na(input$min_quality)) "NULL" else as.character(input$min_quality); paste0("library(TSGenerator)\nlibrary(terra)\n\nqflag <- terra::rast(\"", if(identical(input$q_source,"path")) input$q_path else "path/to/QFLAG.tif", "\")\nsummary <- summarize_quality(qflag, type = \"",input$type,"\")\n\n# Optional masking\ntarget <- terra::rast(\"",input$target_path,"\")\nmasked <- mask_quality(target, qflag, type = \"",input$type,"\", min_quality = ",mq,")") }))
  })
}
