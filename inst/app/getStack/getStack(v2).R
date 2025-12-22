# ============================================================================
# Get_Stack_App
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyFiles)
library(raster)
library(parallel)
library(doParallel)
library(foreach)
library(fs)

# ============================================================================
# UI
# ============================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-layer-group", style = "margin-right: 8px;"),
      "Get Stack",
      style = "font-weight: bold; font-size: 18px;"
    ),
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("⚙️ Configuración",
               tabName = "config",
               icon = icon("cog"),
               badgeLabel = "Primero",
               badgeColor = "orange"),
      menuItem("🛠️ Crear Stack",
               tabName = "stack",
               icon = icon("layer-group"),
               badgeLabel = "Principal",
               badgeColor = "blue"),
      menuItem("📊 Resultados",
               tabName = "results",
               icon = icon("chart-bar"),
               badgeLabel = "Resumen",
               badgeColor = "green"),
      br(),
      div(style = "padding: 15px;",
          h5("💡 Consejos rápidos", style = "color: #fff; font-weight: bold;"),
          p("• Carpetas con archivos .tif", style = "color: #bbb; font-size: 12px;"),
          p("• Nombres deben contener fecha YYYYMMDD", style = "color: #bbb; font-size: 12px;"),
          p("• QFLAG debe tener nombre exacto de fecha", style = "color: #bbb; font-size: 12px;")
      )
    )
  ),

  dashboardBody(
    useShinyjs(),

    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }
        .box {
          border-radius: 15px;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1);
          backdrop-filter: blur(10px);
          border: 1px solid rgba(255,255,255,0.18);
          transition: all 0.3s ease;
          background: rgba(255,255,255,0.95);
        }
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 15px 45px rgba(0,0,0,0.15);
        }
        .box-header {
          border-radius: 15px 15px 0 0;
          background: linear-gradient(45deg, #667eea, #764ba2);
          color: white;
          padding: 15px 20px;
        }
        .box-header h3:before {
          content: '✨';
          margin-right: 10px;
          font-size: 18px;
        }
        .btn {
          border-radius: 25px;
          padding: 10px 25px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 1px;
          transition: all 0.3s ease;
          border: none;
          position: relative;
          overflow: hidden;
        }
        .btn:before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%;
          width: 100%;
          height: 100%;
          background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
          transition: left 0.5s;
        }
        .btn:hover:before { left: 100%; }
        .btn-primary {
          background: linear-gradient(45deg, #667eea, #764ba2);
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
        }
        .btn-success {
          background: linear-gradient(45deg, #56ab2f, #a8e6cf);
          box-shadow: 0 4px 15px rgba(86, 171, 47, 0.4);
        }
        .form-control, .form-group input {
          border-radius: 12px;
          border: 2px solid #e9ecef;
          padding: 12px 15px;
          transition: all 0.3s ease;
          font-size: 14px;
        }
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
          transform: translateY(-1px);
        }
        .control-label {
          font-weight: 600;
          color: #495057;
          margin-bottom: 8px;
          display: flex;
          align-items: center;
        }
        #errorMessages {
          background: linear-gradient(45deg, #ff6b6b, #ee5a52);
          color: white;
          padding: 15px;
          border-radius: 12px;
          margin: 15px 0;
          box-shadow: 0 4px 15px rgba(255, 107, 107, 0.3);
          animation: shake 0.5s ease-in-out;
        }
        @keyframes shake {
          0%, 100% { transform: translateX(0); }
          25% { transform: translateX(-5px); }
          75% { transform: translateX(5px); }
        }
        .status-badge {
          display: inline-block;
          padding: 5px 12px;
          border-radius: 20px;
          font-size: 11px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        .status-success { background: linear-gradient(45deg, #56ab2f, #a8e6cf); color: white; }
        .status-warning { background: linear-gradient(45deg, #f7971e, #ffd200); color: white; }
        .status-error { background: linear-gradient(45deg, #ff6b6b, #ee5a52); color: white; }
        .fade-in { animation: fadeIn 0.5s ease-in; }
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
      "))
    ),

    tabItems(
      # ======================================================================
      # PESTAÑA 1: CONFIGURACIÓN
      # ======================================================================
      tabItem(tabName = "config",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📂 Carpetas de Entrada y Salida",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Seleccione las carpetas con los rasters de IV, QFLAG y la carpeta de salida.",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        fluidRow(
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📥 Carpeta con IV (.tif):", class = "control-label"),
                                     shinyDirButton("iv_folder", "Seleccionar carpeta", "Carpeta con archivos IV")
                                 ),
                                 verbatimTextOutput("iv_path_display")
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📂 Carpeta con QFLAG:", class = "control-label"),
                                     shinyDirButton("qflag_folder", "Seleccionar carpeta", "Carpeta con archivos QFLAG")
                                 ),
                                 verbatimTextOutput("qflag_path_display")
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📤 Carpeta de salida:", class = "control-label"),
                                     shinyDirButton("output_folder", "Seleccionar carpeta", "Carpeta para stacks resultantes")
                                 ),
                                 verbatimTextOutput("output_path_display")
                          )
                        )
                      )
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 2: CREAR STACK
      # ======================================================================
      tabItem(tabName = "stack",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "🛠️ Parámetros para Crear Stack",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Configure opciones para combinar IV + QFLAG en un stack multibanda.",
                          style = "color: #6c757d; margin-bottom: 25px;"),
                        fluidRow(
                          column(6,
                                 div(class = "form-group",
                                     tags$label("🔍 Patrón de archivos IV:", class = "control-label"),
                                     textInput("pattern", "", value = "\\.tif$", placeholder = "\\.tif$")
                                 )
                          ),
                          column(6,
                                 div(class = "form-group",
                                     tags$label("⚡ Usar procesamiento paralelo:", class = "control-label"),
                                     checkboxInput("use_parallel", "Activar paralelización (más rápido)", value = TRUE)
                                 )
                          )
                        ),
                        div(id = "errorMessages"),
                        div(style = "text-align: center; margin-top: 30px;",
                            actionButton("run_stack", "🚀 Crear Stacks IV + QFLAG",
                                         icon = icon("play"),
                                         class = "btn-primary",
                                         style = "width: 80%; font-size: 16px; padding: 15px;")
                        )
                      )
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 3: RESULTADOS
      # ======================================================================
      tabItem(tabName = "results",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📊 Resumen del Procesamiento",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(class = "results-container",
                          withSpinner(
                            verbatimTextOutput("processing_summary"),
                            type = 5,
                            color = "#667eea"
                          )
                      )
                    )
                  )
              )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  # ==========================================================================
  # SELECTOR DE CARPETAS CON TODOS LOS DISCOS LOCALES
  # ==========================================================================
  volumes <- c()
  if (.Platform$OS.type == "windows") {
    drives <- sapply(LETTERS[1:26], function(l) {
      drive <- paste0(l, ":/")
      if (dir.exists(drive)) drive else NULL
    })
    volumes <- unlist(drives)
    names(volumes) <- paste0(volumes, " ")
  } else {
    volumes <- c("Raíz (/)" = "/", "Home" = fs::path_home())
  }

  shinyDirChoose(input, "iv_folder", roots = volumes, allowDirCreate = TRUE)
  shinyDirChoose(input, "qflag_folder", roots = volumes, allowDirCreate = TRUE)
  shinyDirChoose(input, "output_folder", roots = volumes, allowDirCreate = TRUE)

  iv_path <- reactive({
    req(input$iv_folder)
    parseDirPath(roots = volumes, input$iv_folder)
  })

  qflag_path <- reactive({
    req(input$qflag_folder)
    parseDirPath(roots = volumes, input$qflag_folder)
  })

  output_path <- reactive({
    req(input$output_folder)
    parseDirPath(roots = volumes, input$output_folder)
  })

  output$iv_path_display <- renderPrint({
    cat(ifelse(is.null(input$iv_folder), "Ninguna carpeta seleccionada", iv_path()))
  })

  output$qflag_path_display <- renderPrint({
    cat(ifelse(is.null(input$qflag_folder), "Ninguna carpeta seleccionada", qflag_path()))
  })

  output$output_path_display <- renderPrint({
    cat(ifelse(is.null(input$output_folder), "Ninguna carpeta seleccionada", output_path()))
  })

  # ==========================================================================
  # VALORES REACTIVOS
  # ==========================================================================
  values <- reactiveValues(
    processed = 0,
    matched = 0,
    missing_qflag = 0,
    dimension_mismatch = 0,
    log_messages = character()
  )

  # ==========================================================================
  # FUNCIÓN get.Stack() (integrada directamente)
  # ==========================================================================
  get.Stack <- function(IV_path, QFLAG, output_path, pattern = "\\.tif$", use_parallel = TRUE) {
    archivos <- list.files(IV_path, pattern = pattern, full.names = TRUE)
    if (length(archivos) == 0) {
      values$log_messages <<- c(values$log_messages, "⚠️ No se encontraron archivos .tif en la carpeta IV")
      return()
    }

    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

    values$processed <<- length(archivos)
    values$matched <<- 0
    values$missing_qflag <<- 0
    values$dimension_mismatch <<- 0

    process_file <- function(archivo) {
      fecha <- gsub(".*_(\\d{8})_.*", "\\1", basename(archivo))
      archivo_qflag <- file.path(QFLAG, paste0(fecha, ".tif"))  # Asumiendo .tif también

      if (file.exists(archivo_qflag)) {
        raster_iv <- brick(archivo)
        qflag <- brick(archivo_qflag)

        qflag_resampled <- raster::resample(qflag, raster_iv, method = "bilinear")

        if (nrow(raster_iv) == nrow(qflag_resampled) && ncol(raster_iv) == ncol(qflag_resampled)) {
          stack_final <- addLayer(raster_iv, qflag_resampled)
          output_file <- file.path(output_path, paste0("stack_", fecha, ".tif"))
          writeRaster(stack_final, output_file, format = "GTiff", overwrite = TRUE)
          values$matched <<- values$matched + 1
          paste("✅ Stack creado para fecha", fecha)
        } else {
          values$dimension_mismatch <<- values$dimension_mismatch + 1
          paste("❌ Dimensiones no coinciden para fecha", fecha)
        }
      } else {
        values$missing_qflag <<- values$missing_qflag + 1
        paste("❌ QFLAG no encontrado para fecha", fecha)
      }
    }

    if (use_parallel && length(archivos) > 1) {
      cores <- detectCores() - 1
      cl <- makeCluster(cores)
      registerDoParallel(cl)

      results <- foreach(archivo = archivos, .combine = c) %dopar% {
        process_file(archivo)
      }

      stopCluster(cl)
    } else {
      results <- sapply(archivos, process_file)
    }

    values$log_messages <<- c(values$log_messages, results)
  }

  # ==========================================================================
  # EJECUTAR PROCESO
  # ==========================================================================
  observeEvent(input$run_stack, {
    html("errorMessages", "")
    errors <- character()

    if (is.null(input$iv_folder)) errors <- c(errors, "📥 Seleccione carpeta con archivos IV")
    if (is.null(input$qflag_folder)) errors <- c(errors, "📂 Seleccione carpeta con QFLAG")
    if (is.null(input$output_folder)) errors <- c(errors, "📤 Seleccione carpeta de salida")

    if (length(errors) > 0) {
      error_html <- paste(
        "<div class='alert alert-danger' role='alert'>",
        "<h5>⚠️ Errores:</h5><ul>",
        paste("<li>", errors, "</li>", collapse = ""),
        "</ul></div>"
      )
      html("errorMessages", error_html)
      return()
    }

    withProgress(message = '🛠️ Creando stacks IV + QFLAG...', value = 0, {
      incProgress(0.1, detail = "Inicializando...")

      get.Stack(
        IV_path = iv_path(),
        QFLAG = qflag_path(),
        output_path = output_path(),
        pattern = input$pattern,
        use_parallel = input$use_parallel
      )

      incProgress(0.9, detail = "Finalizando...")

      showNotification(
        paste("✅ Procesamiento completado:", values$matched, "stacks creados"),
        type = "message",
        duration = 8
      )
    })
  })

  # ==========================================================================
  # RESUMEN DE RESULTADOS
  # ==========================================================================
  output$processing_summary <- renderPrint({
    cat("📊 RESUMEN DEL PROCESAMIENTO - GET STACK\n")
    cat(rep("=", 55), "\n\n")

    cat("📂 Carpeta IV: ", ifelse(is.null(input$iv_folder), "No seleccionada", iv_path()), "\n")
    cat("📂 Carpeta QFLAG: ", ifelse(is.null(input$qflag_folder), "No seleccionada", qflag_path()), "\n")
    cat("📤 Carpeta salida: ", ifelse(is.null(input$output_folder), "No seleccionada", output_path()), "\n\n")

    if (values$processed > 0) {
      cat("🔢 Archivos IV procesados: ", values$processed, "\n")
      cat("✅ Stacks creados correctamente: ", values$matched, "\n")
      cat("❌ QFLAG no encontrado: ", values$missing_qflag, "\n")
      cat("❌ Dimensiones no coincidentes: ", values$dimension_mismatch, "\n\n")

      if (values$matched > 0) {
        cat("✨ Los stacks se guardaron en:\n   ", output_path(), "\n")
      }

      if (length(values$log_messages) > 0) {
        cat("\n📋 Detalle por archivo:\n")
        cat(paste(values$log_messages, collapse = "\n"), "\n")
      }
    } else {
      cat("📭 Aún no se ha ejecutado ningún procesamiento.\n\n")
      cat("👉 Vaya a la pestaña 'Crear Stack' y haga clic en 'Crear Stacks IV + QFLAG'\n")
    }

    cat("\n⏰ Última ejecución:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================
shinyApp(ui = ui, server = server)
