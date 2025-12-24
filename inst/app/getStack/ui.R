# Cargar script global
source("global.R")

# global.R --------------------------------------------------------------------
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

# Detección de todos los discos locales (para shinyFiles)
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
2. ui.R
R# ui.R ------------------------------------------------------------------------
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
      # PESTAÑA 1: CONFIGURACIÓN
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
      # PESTAÑA 2: CREAR STACK
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
      # PESTAÑA 3: RESULTADOS
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
