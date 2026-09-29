mod_temporal_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("input_status"),width=4), shinydashboard::valueBoxOutput(ns("quality_status"),width=4), shinydashboard::valueBoxOutput(ns("impute_status"),width=4)
    ),
    shiny::fluidRow(
      shinydashboard::box(width=4,title="1. Time-series input",status="primary",solidHeader=TRUE,
        shiny::radioButtons(ns("source"),"Source",c("Spatial Processing result"="spatial","CSV file"="csv"),inline=TRUE),
        shiny::conditionalPanel(sprintf("input['%s'] == 'csv'",ns("source")), shiny::fileInput(ns("csv"),"Time-series CSV",accept=".csv")),
        shiny::uiOutput(ns("column_ui")), shiny::numericInput(ns("step"),"Expected temporal step (days)",10,min=1),
        shiny::actionButton(ns("plan"),"Build temporal plan",icon=shiny::icon("project-diagram"),class="btn-primary btn-block")),
      shinydashboard::box(width=4,title="2. Completeness & quality",status="primary",solidHeader=TRUE,
        shiny::numericInput(ns("window"),"Assessment window (days; 0 = full series)",0,min=0),
        shiny::numericInput(ns("high"),"High completeness threshold",0.90,min=0,max=1,step=.05),
        shiny::numericInput(ns("medium"),"Medium threshold",0.75,min=0,max=1,step=.05),
        shiny::numericInput(ns("low"),"Low threshold",0.50,min=0,max=1,step=.05),
        shiny::actionButton(ns("assess"),"Assess time-series quality",icon=shiny::icon("check-double"),class="btn-success btn-block")),
      shinydashboard::box(width=4,title="3. Optional imputation",status="primary",solidHeader=TRUE,
        shiny::selectInput(ns("method"),"Method",c("Linear"="linear","Kalman (requires imputeTS)"="kalman")),
        shiny::selectInput(ns("series_type"),"Series type",c("Generic / raw series"="generic","ST (processed; guarded)"="st")),
        shiny::checkboxInput(ns("allow_processed"),"Allow additional imputation of processed ST",FALSE),
        shiny::numericInput(ns("max_gap"),"Maximum gap (observations; 0 = unlimited)",0,min=0,step=1),
        shiny::actionButton(ns("impute"),"Run optional imputation",icon=shiny::icon("magic"),class="btn-warning btn-block"))
    ),
    shiny::fluidRow(
      shinydashboard::box(width=6,title="Temporal diagnostics",status="info",solidHeader=TRUE,DT::DTOutput(ns("diag"))),
      shinydashboard::box(width=6,title="Completeness assessment",status="info",solidHeader=TRUE,shinycssloaders::withSpinner(DT::DTOutput(ns("quality")),type=6))
    ),
    shiny::fluidRow(
      shinydashboard::box(width=7,title="Series preview",status="info",solidHeader=TRUE,DT::DTOutput(ns("series"))),
      shinydashboard::box(width=5,title="Reproducible R code",status="info",solidHeader=TRUE,shiny::verbatimTextOutput(ns("rcode")), shiny::downloadButton(ns("download_csv"),"Download current series",class="btn-success btn-block"), shiny::downloadButton(ns("download_code"),"Save R script",class="btn-default btn-block"))
    )
  )
}

mod_temporal_server <- function(id, spatial_result=NULL) {
  shiny::moduleServer(id,function(input,output,session){
    raw <- shiny::reactive({ if(identical(input$source,"spatial")){ shiny::req(spatial_result); z<-spatial_result(); shiny::req(z); as.data.frame(z) } else { shiny::req(input$csv); readr::read_csv(input$csv$datapath,show_col_types=FALSE) } })
    output$column_ui <- shiny::renderUI({ z<-tryCatch(raw(),error=function(e)NULL); if(is.null(z)) return(shiny::tags$p(class="help-block","Load a series to map columns.")); n<-names(z); shiny::tagList(shiny::selectInput(session$ns("id_col"),"ID column",n,selected=if("ID"%in%n)"ID" else n[1]), shiny::selectInput(session$ns("date_col"),"Date column",n,selected=if("Date"%in%n)"Date" else n[min(2,length(n))]), shiny::selectInput(session$ns("value_col"),"Value column",n,selected=if("Value"%in%n)"Value" else n[length(n)])) })
    plan <- shiny::reactiveVal(NULL); miss <- shiny::reactiveVal(NULL); qual <- shiny::reactiveVal(NULL); imp <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$plan,{ z<-tryCatch(TSGenerator::temporal_plan(as.data.frame(raw()),input$id_col,input$date_col,input$value_col,expected_step=input$step),error=function(e)e); if(inherits(z,"error")) shiny::showNotification(conditionMessage(z),type="error",duration=NULL) else {plan(z); miss(tryCatch(TSGenerator::summarize_missingness(z),error=function(e)NULL)); imp(NULL); shiny::showNotification("Temporal plan validated.",type="message")}})
    shiny::observeEvent(input$assess,{ p<-plan(); if(is.null(p)){ shiny::showNotification("Build the temporal plan first.",type="warning"); return() }; th<-c(high=input$high,medium=input$medium,low=input$low); w<-if(input$window<=0)NULL else input$window; z<-tryCatch(TSGenerator::assess_ts_quality(p,window_days=w,thresholds=th),error=function(e)e); if(inherits(z,"error")) shiny::showNotification(conditionMessage(z),type="error",duration=NULL) else {qual(z); shiny::showNotification("Temporal quality assessment complete.",type="message")}})
    shiny::observeEvent(input$impute,{ p<-plan(); if(is.null(p)){ shiny::showNotification("Build the temporal plan first.",type="warning"); return() }; mg<-if(input$max_gap<=0)NULL else as.integer(input$max_gap); z<-tryCatch(TSGenerator::impute_ts(p,method=input$method,series_type=input$series_type,allow_processed=isTRUE(input$allow_processed),max_gap=mg),error=function(e)e); if(inherits(z,"error")) shiny::showNotification(conditionMessage(z),type="error",duration=NULL) else {imp(z); shiny::showNotification(sprintf("Imputation complete: %d value(s) filled.",sum(z$.WasImputed,na.rm=TRUE)),type="message")}})
    output$diag <- DT::renderDT({ p<-plan(); if(is.null(p)) return(DT::datatable(data.frame(Status="Build a temporal plan to view diagnostics."),options=list(dom="t"),rownames=FALSE)); DT::datatable(p$diagnostics,options=list(scrollX=TRUE,pageLength=10),filter="top",rownames=FALSE) })
    output$quality <- DT::renderDT({ z<-qual(); if(is.null(z)) return(DT::datatable(data.frame(Status="Run completeness assessment to view results."),options=list(dom="t"),rownames=FALSE)); DT::datatable(as.data.frame(z),options=list(scrollX=TRUE,pageLength=10),filter="top",rownames=FALSE) })
    current <- shiny::reactive({ if(!is.null(imp())) as.data.frame(imp()) else if(!is.null(plan())) plan()$data else as.data.frame(raw()) })
    output$series <- DT::renderDT(DT::datatable(current(),options=list(scrollX=TRUE,pageLength=10),filter="top",rownames=FALSE))
    output$input_status <- shinydashboard::renderValueBox({z<-tryCatch(raw(),error=function(e)NULL);shinydashboard::valueBox(if(is.null(z))"No data" else paste(nrow(z),"rows"),"Time-series input",icon=shiny::icon("table"),color=if(is.null(z))"yellow" else "aqua")})
    output$quality_status <- shinydashboard::renderValueBox({z<-qual();shinydashboard::valueBox(if(is.null(z))"Pending" else paste(nrow(z),"assessment(s)"),"Temporal quality",icon=shiny::icon("check-double"),color=if(is.null(z))"yellow" else "green")})
    output$impute_status <- shinydashboard::renderValueBox({z<-imp();shinydashboard::valueBox(if(is.null(z))"Not applied" else paste(sum(z$.WasImputed,na.rm=TRUE),"filled"),"Imputation",icon=shiny::icon("magic"),color=if(is.null(z))"yellow" else "green")})
    make_code <- shiny::reactive({
      paste0("library(TSGenerator)\n\nplan <- temporal_plan(data, id_col = \"",input$id_col,"\", date_col = \"",input$date_col,"\", value_col = \"",input$value_col,"\", expected_step = ",input$step,")\nmissingness <- summarize_missingness(plan)\nquality <- assess_ts_quality(plan, window_days = ",if(input$window<=0)"NULL" else input$window,", thresholds = c(high = ",input$high,", medium = ",input$medium,", low = ",input$low,"))\n\n# Optional\nimputed <- impute_ts(plan, method = \"",input$method,"\", series_type = \"",input$series_type,"\", allow_processed = ",toupper(as.character(isTRUE(input$allow_processed))),")")
    })
    output$rcode <- shiny::renderText(make_code())
    output$download_csv <- shiny::downloadHandler(filename=function()"TSGenerator_time_series.csv",content=function(file) utils::write.csv(current(),file,row.names=FALSE))
    output$download_code <- shiny::downloadHandler(filename=function()"TSGenerator_temporal_workflow.R",content=function(file) writeLines(make_code(),file))
    list(plan=shiny::reactive(plan()), quality=shiny::reactive(qual()), series=current, imputed=shiny::reactive(imp()), code=shiny::reactive(make_code()))
  })
}
