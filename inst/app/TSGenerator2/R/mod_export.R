# Results & Export module -------------------------------------------------
# Consolidates outputs produced by upstream GUI modules. No scientific
# processing is implemented here.

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("spatial_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("quality_status"), width = 4),
      shinydashboard::valueBoxOutput(ns("temporal_status"), width = 4)
    ),
    shiny::fluidRow(
      shinydashboard::box(width=4,title="1. Available results",status="primary",solidHeader=TRUE,
        shiny::checkboxGroupInput(ns("include"),"Include in export bundle",
          choices=c("Spatial extraction"="spatial","QFLAG summary"="quality","Temporal series"="series","Temporal quality"="temporal_quality"),
          selected=c("spatial","quality","series","temporal_quality")),
        shiny::textInput(ns("project_name"),"Project / analysis name","TSGenerator_analysis"),
        shiny::tags$p(class="help-block","Only results already produced in the current Shiny session are exported.")),
      shinydashboard::box(width=4,title="2. Reproducibility",status="primary",solidHeader=TRUE,
        shiny::checkboxInput(ns("include_session"),"Include R session information",TRUE),
        shiny::checkboxInput(ns("include_script"),"Include reproducible R workflow",TRUE),
        shiny::checkboxInput(ns("include_manifest"),"Include analysis manifest",TRUE),
        shiny::actionButton(ns("refresh"),"Refresh result inventory",icon=shiny::icon("sync"),class="btn-primary btn-block")),
      shinydashboard::box(width=4,title="3. Export",status="primary",solidHeader=TRUE,
        shiny::downloadButton(ns("bundle"),"Download analysis bundle (.zip)",class="btn-success btn-block"),
        shiny::downloadButton(ns("script"),"Download R workflow",class="btn-default btn-block"),
        shiny::downloadButton(ns("manifest"),"Download manifest",class="btn-default btn-block"),
        shiny::tags$p(class="help-block","The bundle contains data products and provenance metadata; it does not rerun analyses."))
    ),
    shiny::fluidRow(
      shinydashboard::box(width=6,title="Result inventory",status="info",solidHeader=TRUE,DT::DTOutput(ns("inventory"))),
      shinydashboard::box(width=6,title="Selected result preview",status="info",solidHeader=TRUE,
        shiny::selectInput(ns("preview_type"),"Preview",choices=c("Spatial extraction"="spatial","QFLAG summary"="quality","Temporal series"="series","Temporal quality"="temporal_quality")),
        DT::DTOutput(ns("preview")))
    ),
    shiny::fluidRow(
      shinydashboard::box(width=8,title="Reproducible R workflow",status="info",solidHeader=TRUE,shiny::verbatimTextOutput(ns("workflow"))),
      shinydashboard::box(width=4,title="Provenance",status="info",solidHeader=TRUE,shiny::verbatimTextOutput(ns("provenance")))
    )
  )
}

mod_export_server <- function(id, acquisition_state = NULL, spatial_state, quality_state, temporal_state) {
  shiny::moduleServer(id, function(input, output, session) {
    safe <- function(expr) tryCatch(expr, error=function(e) NULL)
    results <- shiny::reactive({
      list(
        spatial = safe(spatial_state$extraction()),
        quality = safe(quality_state$summary()),
        series = safe(temporal_state$series()),
        temporal_quality = safe(temporal_state$quality())
      )
    })
    inventory <- shiny::reactive({
      z <- results()
      labels <- c(spatial="Spatial extraction",quality="QFLAG summary",series="Temporal series",temporal_quality="Temporal quality")
      data.frame(Result=unname(labels), Available=vapply(z,function(x)!is.null(x),logical(1)), Rows=vapply(z,function(x)if(is.null(x))0L else NROW(as.data.frame(x)),integer(1)), stringsAsFactors=FALSE)
    })
    output$inventory <- DT::renderDT(DT::datatable(inventory(),options=list(dom="t",paging=FALSE),rownames=FALSE))
    output$preview <- DT::renderDT({ z<-results()[[input$preview_type]]; if(is.null(z)) return(DT::datatable(data.frame(Status="This result has not been generated in the current session."),options=list(dom="t"),rownames=FALSE)); DT::datatable(as.data.frame(z),options=list(scrollX=TRUE,pageLength=10),filter="top",rownames=FALSE) })
    output$spatial_status <- shinydashboard::renderValueBox({z<-results()$spatial;shinydashboard::valueBox(if(is.null(z))"Pending" else paste(NROW(as.data.frame(z)),"rows"),"Spatial result",icon=shiny::icon("layer-group"),color=if(is.null(z))"yellow" else "green")})
    output$quality_status <- shinydashboard::renderValueBox({z<-results()$quality;shinydashboard::valueBox(if(is.null(z))"Pending" else paste(NROW(as.data.frame(z)),"rows"),"QFLAG result",icon=shiny::icon("check-circle"),color=if(is.null(z))"yellow" else "green")})
    output$temporal_status <- shinydashboard::renderValueBox({z<-results()$series;shinydashboard::valueBox(if(is.null(z))"Pending" else paste(NROW(as.data.frame(z)),"rows"),"Temporal result",icon=shiny::icon("chart-line"),color=if(is.null(z))"yellow" else "green")})

    workflow_text <- shiny::reactive({
      blocks <- c("# TSGenerator 2.0 reproducible workflow", paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "")
      ac <- NULL
      if (!is.null(acquisition_state)) {
        pv <- safe(acquisition_state$preview())
        dl <- safe(acquisition_state$download())
        if (!is.null(pv) || !is.null(dl)) ac <- safe(acquisition_state$code())
      }
      sc <- safe(spatial_state$code()); qc <- safe(quality_state$code()); tc <- safe(temporal_state$code())
      if(!is.null(ac)) blocks <- c(blocks,"# ---- Data Acquisition ----",ac,"")
      if(!is.null(sc)) blocks <- c(blocks,"# ---- Spatial Processing ----",sc,"")
      if(!is.null(qc) && !is.null(results()$quality)) blocks <- c(blocks,"# ---- Product Quality ----",qc,"")
      if(!is.null(tc) && !is.null(temporal_state$plan())) blocks <- c(blocks,"# ---- Time-Series Analysis ----",tc,"")
      paste(blocks,collapse="\n")
    })
    output$workflow <- shiny::renderText(workflow_text())
    manifest_text <- shiny::reactive({
      inv <- inventory()
      acq_line <- "Acquisition: not run in this session"
      if (!is.null(acquisition_state)) {
        dl <- safe(acquisition_state$download())
        pv <- safe(acquisition_state$preview())
        fam <- safe(acquisition_state$product())
        if (!is.null(dl)) {
          acq_line <- paste0("Acquisition: ", fam, " download completed")
        } else if (!is.null(pv)) {
          acq_line <- paste0("Acquisition: ", fam, " preview completed")
        }
      }
      paste(c(
        "TSGenerator analysis manifest",
        paste0("Project: ", input$project_name),
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        paste0("TSGenerator: ", as.character(utils::packageVersion("TSGenerator"))),
        paste0("R: ", R.version.string),
        acq_line, "",
        apply(inv,1,function(r) paste0(r[[1]], ": ", if(r[[2]]=="TRUE") paste0("available (",r[[3]]," rows)") else "not available"))
      ),collapse="\n")
    })
    output$provenance <- shiny::renderText(manifest_text())
    output$script <- shiny::downloadHandler(filename=function() paste0(gsub("[^A-Za-z0-9_-]+","_",input$project_name),"_workflow.R"),content=function(file) writeLines(workflow_text(),file))
    output$manifest <- shiny::downloadHandler(filename=function() paste0(gsub("[^A-Za-z0-9_-]+","_",input$project_name),"_manifest.txt"),content=function(file) writeLines(manifest_text(),file))
    output$bundle <- shiny::downloadHandler(filename=function() paste0(gsub("[^A-Za-z0-9_-]+","_",input$project_name),"_TSGenerator_bundle.zip"), content=function(file){
      td<-tempfile("tsg_export_");dir.create(td); z<-results(); sel<-input$include
      map<-c(spatial="spatial_extraction.csv",quality="qflag_summary.csv",series="time_series.csv",temporal_quality="temporal_quality.csv")
      for(nm in intersect(names(z),sel)) if(!is.null(z[[nm]])) utils::write.csv(as.data.frame(z[[nm]]),file.path(td,map[[nm]]),row.names=FALSE)
      if(isTRUE(input$include_script)) writeLines(workflow_text(),file.path(td,"workflow.R"))
      if(isTRUE(input$include_manifest)) writeLines(manifest_text(),file.path(td,"manifest.txt"))
      if(isTRUE(input$include_session)) capture.output(utils::sessionInfo(),file=file.path(td,"sessionInfo.txt"))
      old<-setwd(td);on.exit(setwd(old),add=TRUE); files<-list.files(td); shiny::validate(shiny::need(length(files)>0,"No generated results are available for export.")); utils::zip(zipfile=file,files=files)
    })
  })
}
