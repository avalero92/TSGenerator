# Cargar script global
source("global.R")

# ui.R ------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-edit", style = "margin-right: 8px;"),
      "Rename Times Series VI",
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
      menuItem("🖼️ Renombrar Imágenes",
               tabName = "rename",
               icon = icon("edit"),
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
          p("• Archivos deben contener fecha YYYYMMDD", style = "color: #bbb; font-size: 12px;"),
          p("• Ej: S2A_..._20230115_...", style = "color: #bbb; font-size: 12px;"),
          p("• Se renombrará a 20230115.tif", style = "color: #bbb; font-size: 12px;")
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
        .form-control {
          border-radius: 12px;
          border: 2px solid #e9ecef;
          padding: 12px 15px;
          transition: all 0.3s ease;
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
        .results-container {
          background: rgba(255,255,255,0.95);
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        }
        .fade-in {
          animation: fadeIn 0.5s ease-in;
        }
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
                      title = "📂 Seleccionar Carpeta de Imágenes",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Seleccione la carpeta que contiene los archivos .tif con nombres largos (que incluyen fecha en formato YYYYMMDD).",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        fluidRow(
                          column(12,
                                 div(class = "form-group",
                                     tags$label("📁 Carpeta con imágenes TIF:", class = "control-label"),
                                     shinyDirButton("input_folder", "Seleccionar carpeta", "Elija la carpeta con imágenes"),
                                     verbatimTextOutput("input_path_display")
                                 )
                          )
                        )
                      )
                    )
                  )
              )
      ),
      # PESTAÑA 2: RENOMBRAR IMÁGENES
      tabItem(tabName = "rename",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "🖼️ Renombrar Serie Temporal VI",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Esta herramienta renombrará todos los archivos .tif de la carpeta seleccionada usando solo la fecha (YYYYMMDD).",
                          style = "color: #6c757d; margin-bottom: 25px;"),
                        p("Ejemplo: S2A_MSIL2A_20230115T105231_... → 20230115.tif",
                          style = "font-style: italic; color: #495057;"),
                        div(id = "errorMessages"),
                        div(style = "text-align: center; margin-top: 30px;",
                            actionButton("run_rename", "🚀 Renombrar Imágenes",
                                         icon = icon("edit"),
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
                      title = "📊 Resumen del Renombrado",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(class = "results-container",
                          withSpinner(
                            verbatimTextOutput("rename_summary"),
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
