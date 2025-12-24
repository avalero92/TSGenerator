# Cargar script global
source("global.R")

ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-satellite", style = "margin-right: 8px;"),
      "Download HRVPP",
      style = "font-weight: bold; font-size: 18px;"
    ),
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("🔍 Búsqueda y Descarga",
               tabName = "search",
               icon = icon("search"),
               badgeLabel = "Principal",
               badgeColor = "blue"),
      menuItem("🖼️ Visualización",
               tabName = "visualization",
               icon = icon("image"),
               badgeLabel = "Nuevo",
               badgeColor = "green"),
      menuItem("⚙️ Configuración",
               tabName = "config",
               icon = icon("cog"),
               badgeLabel = "Sistema",
               badgeColor = "orange"),
      br(),
      div(style = "padding: 15px;",
          h5("💡 Consejos", style = "color: #fff; font-weight: bold;"),
          p("• Configure Python antes de buscar", style = "color: #bbb; font-size: 12px;"),
          p("• Use fechas en formato YYYY-MM-DD", style = "color: #bbb; font-size: 12px;"),
          p("• Las coordenadas deben estar en WGS84", style = "color: #bbb; font-size: 12px;")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* CSS COMPLETAMENTE IDÉNTICO A DownloadVI */
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
        .box-header h3 {
          font-weight: 600;
          margin: 0;
          display: flex;
          align-items: center;
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
        .btn:hover:before {
          left: 100%;
        }
        .btn-primary {
          background: linear-gradient(45deg, #667eea, #764ba2);
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
        }
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(102, 126, 234, 0.6);
        }
        .btn-success {
          background: linear-gradient(45deg, #56ab2f, #a8e6cf);
          box-shadow: 0 4px 15px rgba(86, 171, 47, 0.4);
        }
        .btn-success:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(86, 171, 47, 0.6);
        }
        .form-control, .form-group input {
          border-radius: 12px;
          border: 2px solid #e9ecef;
          padding: 12px 15px;
          transition: all 0.3s ease;
          font-size: 14px;
        }
        .form-control:focus, .form-group input:focus {
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
        .thumbnail {
          display: inline-block;
          margin: 15px;
          border-radius: 15px;
          padding: 10px;
          background: rgba(255,255,255,0.9);
          backdrop-filter: blur(10px);
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          cursor: pointer;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        }
        .thumbnail:hover {
          transform: scale(1.08) rotate(2deg);
          box-shadow: 0 15px 35px rgba(0,0,0,0.2);
          z-index: 10;
          position: relative;
        }
        .thumbnail img {
          width: 180px;
          height: 120px;
          object-fit: cover;
          border-radius: 10px;
          transition: all 0.3s ease;
        }
        .thumbnail-caption {
          text-align: center;
          margin-top: 10px;
          font-size: 12px;
          font-weight: 600;
          color: #495057;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 180px;
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
        .status-badge {
          display: inline-block;
          padding: 5px 12px;
          border-radius: 20px;
          font-size: 11px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        .status-success {
          background: linear-gradient(45deg, #56ab2f, #a8e6cf);
          color: white;
        }
        .status-error {
          background: linear-gradient(45deg, #ff6b6b, #ee5a52);
          color: white;
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
      tabItem(tabName = "search",
              div(class = "fade-in",
                  fluidRow(
                    box(title = "🔐 Credenciales HDA", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
                        div(
                          p("Ingrese sus credenciales para acceder a la API de HDA:", style = "color: #6c757d; margin-bottom: 20px;"),
                          div(class = "form-group",
                              tags$label("👤 Usuario:", class = "control-label"),
                              textInput("user", "", placeholder = "1234")
                          ),
                          div(class = "form-group",
                              tags$label("🔑 Contraseña:", class = "control-label"),
                              passwordInput("password", "", placeholder = "Su contraseña segura")
                          ),
                          tags$small("Sus credenciales se mantienen seguras y no se almacenan.", style = "color: #28a745; font-style: italic;")
                        )
                    ),
                    box(title = "⚙️ Configuración del Sistema", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
                        div(
                          p("Configure las rutas necesarias para el funcionamiento:", style = "color: #6c757d; margin-bottom: 20px;"),
                          div(class = "form-group",
                              tags$label("🐍 Ruta de Python:", class = "control-label"),
                              textInput("ruta_python", "", placeholder = "/usr/bin/python3 o C:/Python39/python.exe")
                          ),
                          div(class = "form-group",
                              tags$label("📁 Carpeta de Descarga:", class = "control-label"),
                              shinyDirButton("download_dir", "Seleccionar carpeta", "Elija la carpeta de descarga"),
                              verbatimTextOutput("download_path_display")
                          ),
                          div(style = "text-align: center; margin-top: 20px;",
                              actionButton("check_config", "✅ Verificar Configuración", icon = icon("check"), class = "btn-success", style = "width: 100%;")
                          )
                        )
                    )
                  ),
                  fluidRow(
                    box(title = "🔍 Parámetros de Búsqueda Avanzada", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                        div(
                          p("Configure los criterios para filtrar los productos HRVPP:", style = "color: #6c757d; margin-bottom: 25px;"),
                          fluidRow(
                            column(4,
                                   div(class = "form-group",
                                       tags$label("📊 ID del Dataset:", class = "control-label"),
                                       textInput("dataset_id", "EO:EEA:DAT:CLMS_HRVPP_VPP", placeholder = "EO:EEA:DAT:CLMS_HRVPP_VPP")
                                   )
                            ),
                            column(4,
                                   div(class = "form-group",
                                       tags$label("🛰️ Product Type:", class = "control-label"),
                                       textInput("productType", "", placeholder = "Ej: SOSD")
                                   )
                            ),
                            column(4,
                                   div(class = "form-group",
                                       tags$label("🗂️ Product Group ID:", class = "control-label"),
                                       textInput("productGroupId", "", placeholder = "Ej: s1")
                                   )
                            )
                          ),
                          fluidRow(
                            column(4,
                                   div(class = "form-group",
                                       tags$label("🧩 Tile ID:", class = "control-label"),
                                       textInput("tileId", "", placeholder = "Ej: 30TYN")
                                   )
                            ),
                            column(4,
                                   div(class = "form-group",
                                       tags$label("📅 Fecha de Inicio:", class = "control-label"),
                                       textInput("start", "", placeholder = "2023-01-01")
                                   )
                            ),
                            column(4,
                                   div(class = "form-group",
                                       tags$label("📅 Fecha de Fin:", class = "control-label"),
                                       textInput("end", "", placeholder = "2023-12-31")
                                   )
                            )
                          ),
                          fluidRow(
                            column(12,
                                   div(class = "form-group",
                                       tags$label("🗺️ Área de Interés (Bbox):", class = "control-label"),
                                       textInput("bbox", "", placeholder = "-3.8,40.3,-3.6,40.5 (xmin,ymin,xmax,ymax)"),
                                       tags$small("Formato: xmin,ymin,xmax,ymax en WGS84", style = "color: #17a2b8; font-style: italic;")
                                   )
                            )
                          ),
                          div(id = "errorMessages"),
                          div(style = "text-align: center; margin-top: 30px;",
                              fluidRow(
                                column(6,
                                       actionButton("search", "🔍 Buscar Productos", icon = icon("search"), class = "btn-primary", style = "width: 90%; margin-right: 5%;")
                                ),
                                column(6,
                                       actionButton("download", "⬇️ Descargar Resultados", icon = icon("download"), class = "btn-success", style = "width: 90%; margin-left: 5%;")
                                )
                              )
                          )
                        )
                    )
                  ),
                  fluidRow(
                    box(title = "📋 Resultados de la Búsqueda", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                        div(class = "results-container",
                            withSpinner(verbatimTextOutput("result"), type = 5, color = "#667eea", size = 0.8)
                        )
                    )
                  )
              )
      ),
      tabItem(tabName = "visualization",
              div(class = "fade-in",
                  fluidRow(
                    box(title = "🖼️ Galería de Imágenes Descargadas", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                        div(
                          p("Haga clic en cualquier miniatura para ver la imagen en detalle:", style = "color: #6c757d; margin-bottom: 20px; text-align: center;"),
                          withSpinner(uiOutput("thumbnails"), type = 6, color = "#667eea")
                        )
                    )
                  ),
                  fluidRow(
                    box(title = "🔍 Visualización Detallada", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                        div(
                          withSpinner(plotOutput("selected_image", height = "600px"), type = 4, color = "#667eea")
                        )
                    )
                  )
              )
      ),
      tabItem(tabName = "config",
              div(class = "fade-in",
                  fluidRow(
                    box(title = "📊 Estado del Sistema", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                        div(
                          p("Información detallada sobre el estado actual del sistema:", style = "color: #6c757d; margin-bottom: 20px;"),
                          div(class = "results-container",
                              withSpinner(verbatimTextOutput("system_info"), type = 7, color = "#667eea")
                          )
                        )
                    )
                  ),
                  fluidRow(
                    box(title = "ℹ️ Información de la Aplicación", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                        div(
                          h4("📖 Acerca de Download HRVPP", style = "color: #495057; margin-bottom: 15px;"),
                          p("Aplicación Shiny para búsqueda, descarga y visualización de productos HRVPP (High Resolution Vegetation Parameters) a través de la API HDA.", style = "text-align: justify;"),
                          h5("🚀 Características principales:", style = "color: #495057; margin-top: 20px;"),
                          tags$ul(
                            tags$li("✅ Interfaz intuitiva y moderna"),
                            tags$li("🔒 Gestión segura de credenciales"),
                            tags$li("🔍 Búsqueda avanzada con múltiples filtros"),
                            tags$li("📊 Validación robusta de entrada"),
                            tags$li("⬇️ Descarga automática con progreso"),
                            tags$li("🖼️ Visualización interactiva de imágenes"),
                            tags$li("📱 Diseño completamente responsivo")
                          ),
                          h5("🛠️ Requisitos del sistema:", style = "color: #495057; margin-top: 20px;"),
                          tags$ul(
                            tags$li("R ≥ 4.0"),
                            tags$li("Python ≥ 3.6 con paquete 'hda' instalado"),
                            tags$li("Credenciales válidas para HDA"),
                            tags$li("Conexión a internet estable")
                          )
                        )
                    )
                  )
              )
      )
    )
  )
)
