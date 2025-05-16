# Cargar script global
source("global.R")

# Definición de UI con estilo mejorado
ui <- dashboardPage(
  dashboardHeader(title = "DownloadVI"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Búsqueda y Descarga", tabName = "search", icon = icon("search")),
      menuItem("Visualización", tabName = "visualization", icon = icon("image")),
      menuItem("Configuración", tabName = "config", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-top: 3px solid #007bff;
        }
        .thumbnail {
          display: inline-block;
          margin: 10px;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 5px;
          transition: transform 0.2s;
        }
        .thumbnail:hover {
          transform: scale(1.05);
          box-shadow: 0 0 10px rgba(0,0,0,0.2);
        }
        .thumbnail img {
          width: 150px;
          height: auto;
        }
        .thumbnail-caption {
          text-align: center;
          margin-top: 5px;
          font-size: 0.8em;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 150px;
        }
        #errorMessages {
          color: #dc3545;
          margin: 10px 0;
        }
        .info-box {
          margin-bottom: 15px;
        }
      "))
    ),
    
    tabItems(
      # Pestaña de Búsqueda y Descarga
      tabItem(tabName = "search",
              fluidRow(
                box(
                  title = "Credenciales", status = "primary", solidHeader = TRUE, width = 6,
                  textInput("user", "Usuario:"),
                  passwordInput("password", "Contraseña:")
                ),
                box(
                  title = "Configuración", status = "primary", solidHeader = TRUE, width = 6,
                  textInput("ruta_python", "Ruta de Python:"),
                  textInput("download_path", "Ruta de Descarga:"),
                  actionButton("check_config", "Verificar Configuración",
                               icon = icon("check"), class = "btn-success")
                )
              ),
              
              fluidRow(
                box(
                  title = "Parámetros de Búsqueda", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4, textInput("dataset_id", "ID del Dataset:")),
                    column(4, textInput("productType", "Tipo de Producto:")),
                    column(4, textInput("platformSerialIdentifier", "Identificador de Plataforma:"))
                  ),
                  fluidRow(
                    column(4, textInput("tileId", "ID del Tile:")),
                    column(4, textInput("start", "Fecha de Inicio (YYYY-MM-DD):")),
                    column(4, textInput("end", "Fecha de Fin (YYYY-MM-DD):"))
                  ),
                  fluidRow(
                    column(12, textInput("bbox", "Bbox (xmin,ymin,xmax,ymax):"))
                  ),
                  fluidRow(
                    column(12,
                           div(id = "errorMessages"),
                           actionButton("search", "Buscar", icon = icon("search"), class = "btn-primary"),
                           actionButton("download", "Descargar", icon = icon("download"), class = "btn-success")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Resultados de Búsqueda", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(verbatimTextOutput("result"))
                )
              )
      ),
      
      # Pestaña de Visualización
      tabItem(tabName = "visualization",
              fluidRow(
                box(
                  title = "Imágenes Descargadas", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(uiOutput("thumbnails"))
                )
              ),
              fluidRow(
                box(
                  title = "Visualización Detallada", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(plotOutput("selected_image", height = "500px"))
                )
              )
      ),
      
      # Pestaña de Configuración
      tabItem(tabName = "config",
              fluidRow(
                box(
                  title = "Estado del Sistema", status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("system_info")
                )
              )
      )
    )
  )
)