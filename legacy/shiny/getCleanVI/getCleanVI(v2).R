# ============================================================================
# QFLAG2_Mask_App - Interfaz Shiny para la función QFLAG2.Mask()
# Autor: Alexey Valero Jorge
# Fecha: 2025
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(terra)     # Recomendado
library(raster)    # Fallback
library(fs)        # Para manejo robusto de rutas
library(shinyFiles)

# ============================================================================
# INTERFAZ DE USUARIO (UI)
# ============================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-mask", style = "margin-right: 8px;"),
      "QFLAG2 Mask",
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
      menuItem("🛠️ Aplicar Máscara",
               tabName = "mask",
               icon = icon("mask"),
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
          p("• Carpetas con archivos .tif multibanda", style = "color: #bbb; font-size: 12px;"),
          p("• Valores típicos: 1, 1024, 2048, 4096, 8192", style = "color: #bbb; font-size: 12px;"),
          p("• 'terra' es más rápido y recomendado", style = "color: #bbb; font-size: 12px;")
      )
    )
  ),

  dashboardBody(
    useShinyjs(),

    # ==========================================================================
    # ESTILOS CSS - IGUALES QUE EN DownloadVI
    # ==========================================================================
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
                        p("Seleccione las carpetas donde están los rasters originales y donde quiere guardar los resultados enmascarados.",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        fluidRow(
                          column(6,
                                 div(class = "form-group",
                                     tags$label("📥 Carpeta con rasters multibanda:", class = "control-label"),
                                     shinyDirButton("stack_folder", "Seleccionar carpeta", "Elija la carpeta de entrada")
                                 ),
                                 verbatimTextOutput("stack_path_display")
                          ),
                          column(6,
                                 div(class = "form-group",
                                     tags$label("📤 Carpeta de salida:", class = "control-label"),
                                     shinyDirButton("output_folder", "Seleccionar carpeta", "Elija la carpeta de salida")
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
      # PESTAÑA 2: APLICAR MÁSCARA
      # ======================================================================
      tabItem(tabName = "mask",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "🛠️ Parámetros de Enmascarado QFLAG2",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Configure los parámetros para aplicar la máscara basada en la banda QFLAG2.",
                          style = "color: #6c757d; margin-bottom: 25px;"),

                        fluidRow(
                          column(4,
                                 div(class = "form-group",
                                     tags$label("🔢 Valores válidos de QFLAG2:", class = "control-label"),
                                     textInput("valid_values", "", value = "1, 1024, 2048, 4096, 8192",
                                               placeholder = "Separados por comas")
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📑 Banda QFLAG2:", class = "control-label"),
                                     numericInput("qflag_band", "", value = 2, min = 1)
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("🎯 Banda objetivo (a enmascarar):", class = "control-label"),
                                     numericInput("target_band", "", value = 1, min = 1)
                                 )
                          )
                        ),

                        fluidRow(
                          column(4,
                                 div(class = "form-group",
                                     tags$label("🔍 Patrón de archivos:", class = "control-label"),
                                     textInput("pattern", "", value = "\\.tif$", placeholder = "\\.tif$")
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📦 Paquete a usar:", class = "control-label"),
                                     selectInput("use_terra", "",
                                                 choices = c("terra (recomendado)" = TRUE, "raster (fallback)" = FALSE),
                                                 selected = TRUE)
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📢 Verbosidad:", class = "control-label"),
                                     checkboxInput("verbose", "Mostrar mensajes detallados", value = TRUE)
                                 )
                          )
                        ),

                        div(id = "errorMessages"),

                        div(style = "text-align: center; margin-top: 30px;",
                            actionButton("run_mask", "🚀 Aplicar Máscara QFLAG2",
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
  # Selección de carpetas (shinyDirs)
  # ==========================================================================
  shinyDirChoose(input, "stack_folder", roots = c(home = fs::path_home()))
  shinyDirChoose(input, "output_folder", roots = c(home = fs::path_home()))

  stack_folder_path <- reactive({
    if (is.null(input$stack_folder)) return(NULL)
    parseDirPath(roots = c(home = fs::path_home()), input$stack_folder)
  })

  output_folder_path <- reactive({
    if (is.null(input$output_folder)) return(NULL)
    parseDirPath(roots = c(home = fs::path_home()), input$output_folder)
  })

  output$stack_path_display <- renderPrint({
    cat(ifelse(is.null(stack_folder_path()), "Ninguna carpeta seleccionada", stack_folder_path()))
  })

  output$output_path_display <- renderPrint({
    cat(ifelse(is.null(output_folder_path()), "Ninguna carpeta seleccionada", output_folder_path()))
  })

  # ==========================================================================
  # Valores reactivos para almacenar resultados
  # ==========================================================================
  values <- reactiveValues(
    last_result = NULL,
    processing_log = ""
  )

  # ==========================================================================
  # Ejecutar la función QFLAG2.Mask()
  # ==========================================================================
  observeEvent(input$run_mask, {
    # Limpiar errores previos
    html("errorMessages", "")

    # Validaciones
    errors <- character()

    if (is.null(stack_folder_path()) || stack_folder_path() == "") {
      errors <- c(errors, "📥 Debe seleccionar una carpeta de entrada")
    }
    if (is.null(output_folder_path()) || output_folder_path() == "") {
      errors <- c(errors, "📤 Debe seleccionar una carpeta de salida")
    }

    valid_vals <- tryCatch({
      as.numeric(unlist(strsplit(trimws(input$valid_values), ",")))
    }, error = function(e) NULL)

    if (is.null(valid_vals) || length(valid_vals) == 0) {
      errors <- c(errors, "🔢 Valores válidos deben ser números separados por comas")
    }

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

    # Ejecutar con progreso
    withProgress(message = '🛠️ Aplicando máscara QFLAG2...', value = 0, {
      incProgress(0.1, detail = "Preparando parámetros...")

      result <- tryCatch({
        QFLAG2.Mask(
          stack_folder = stack_folder_path(),
          output_folder = output_folder_path(),
          valid_values = valid_vals,
          pattern = input$pattern,
          qflag_band = input$qflag_band,
          target_band = input$target_band,
          use_terra = as.logical(input$use_terra),
          verbose = input$verbose
        )
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
        NULL
      })

      incProgress(0.9, detail = "Finalizando...")

      values$last_result <- result

      if (!is.null(result)) {
        showNotification(
          paste("✅ Procesamiento completado:", result$processed, "archivos procesados"),
          type = "message",
          duration = 8
        )
      }
    })
  })

  # ==========================================================================
  # Mostrar resumen de resultados
  # ==========================================================================
  output$processing_summary <- renderPrint({
    if (is.null(values$last_result)) {
      cat("📭 Aún no se ha ejecutado ningún procesamiento.\n\n")
      cat("👉 Vaya a la pestaña 'Aplicar Máscara' y haga clic en 'Aplicar Máscara QFLAG2'\n")
    } else {
      res <- values$last_result
      cat("📊 RESUMEN DEL ÚLTIMO PROCESAMIENTO\n")
      cat(rep("=", 50), "\n\n")
      cat("📂 Carpeta entrada: ", res$stack_folder %||% stack_folder_path(), "\n")
      cat("📁 Carpeta salida:  ", res$output_folder, "\n\n")
      cat("🔢 Archivos encontrados:     ", res$total_files, "\n")
      cat("✅ Procesados correctamente:", res$processed, "\n")
      cat("⏭️  Saltados:                ", res$skipped, "\n")
      cat("❌ Fallidos:                 ", res$failed, "\n\n")

      if (res$processed > 0) {
        cat("✨ Los archivos enmascarados se guardaron en:\n")
        cat("   ", res$output_folder, "\n")
      }

      if (res$failed > 0) {
        cat("\n⚠️  Revise las advertencias en la consola para detalles de errores.\n")
      }

      cat("\n⏰ Procesamiento finalizado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    }
  })
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================
shinyApp(ui = ui, server = server)
