# ============================================================================
# DownloadVI - Aplicación Shiny para Descarga de Imágenes Satelitales
# ============================================================================
#
# Descripción:
# Esta aplicación permite buscar, descargar y visualizar imágenes satelitales
# utilizando la API de HDA (Harmonized Data Access). Proporciona una interfaz
# gráfica intuitiva para gestionar credenciales, configurar parámetros de
# búsqueda y visualizar las imágenes descargadas.
#
# Características principales:
# - Gestión de credenciales HDA
# - Búsqueda parametrizada de imágenes satelitales
# - Descarga automática con seguimiento de progreso
# - Visualización de miniaturas e imágenes completas
# - Validación de entrada robusta
# - Interfaz moderna y responsiva
#
# Autor: [Tu nombre]
# Fecha: [Fecha]
# Versión: 2.0
# ============================================================================

# Carga de librerías requeridas
library(shiny)          # Framework principal para aplicaciones web
library(reticulate)     # Interfaz R-Python
library(magick)         # Procesamiento de imágenes
library(shinydashboard) # Componentes de dashboard
library(shinyjs)        # Funcionalidades JavaScript avanzadas
library(shinycssloaders)# Indicadores de carga animados

# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

#' Configurar el entorno Python
#'
#' Esta función configura el intérprete de Python especificado y valida
#' que esté disponible para su uso con reticulate.
#'
#' @param ruta_python String con la ruta al ejecutable de Python
#' @return Boolean indicando éxito (TRUE) o fallo (FALSE)
#' @examples
#' configurar_python("/usr/bin/python3")
#' configurar_python("C:/Python38/python.exe")
configurar_python <- function(ruta_python) {
  tryCatch({
    if (file.exists(ruta_python)) {
      use_python(ruta_python, required = TRUE)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    message("Error configurando Python: ", e$message)
    return(FALSE)
  })
}

#' Validar entradas del usuario
#'
#' Realiza validación comprehensiva de todos los campos de entrada,
#' incluyendo formato de fechas, bbox y campos obligatorios.
#'
#' @param input Lista con los valores de entrada de Shiny
#' @return Vector de caracteres con mensajes de error (vacío si no hay errores)
validate_inputs <- function(input) {
  errors <- character()

  # Validación de campos obligatorios
  if (input$user == "") errors <- c(errors, "📧 Usuario es requerido")
  if (input$password == "") errors <- c(errors, "🔐 Contraseña es requerida")
  if (input$dataset_id == "") errors <- c(errors, "📊 ID del Dataset es requerido")
  if (input$download_path == "") errors <- c(errors, "📁 Ruta de descarga es requerida")
  if (input$ruta_python == "") errors <- c(errors, "🐍 Ruta de Python es requerida")

  # Validación de formato de fechas (YYYY-MM-DD)
  if (input$start != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$start)) {
      errors <- c(errors, "📅 Formato de fecha de inicio inválido (debe ser YYYY-MM-DD)")
    }
  }

  if (input$end != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$end)) {
      errors <- c(errors, "📅 Formato de fecha de fin inválido (debe ser YYYY-MM-DD)")
    }
  }

  # Validación de bbox (bounding box)
  if (input$bbox != "") {
    bbox_values <- tryCatch({
      as.numeric(unlist(strsplit(input$bbox, ",")))
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(bbox_values) || length(bbox_values) != 4) {
      errors <- c(errors, "🗺️ Formato de Bbox inválido (debe ser: xmin,ymin,xmax,ymax)")
    } else {
      # Validación adicional de rangos de coordenadas
      if (any(abs(bbox_values[c(1,3)]) > 180) || any(abs(bbox_values[c(2,4)]) > 90)) {
        errors <- c(errors, "🌍 Coordenadas fuera de rango válido (lon: ±180, lat: ±90)")
      }
    }
  }

  return(errors)
}

# ============================================================================
# DEFINICIÓN DE LA INTERFAZ DE USUARIO
# ============================================================================

ui <- dashboardPage(
  # Encabezado del dashboard
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-satellite", style = "margin-right: 8px;"),
      "DownloadVI",
      style = "font-weight: bold; font-size: 18px;"
    ),
    titleWidth = 250
  ),

  # Barra lateral con navegación
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
      # Información adicional en la barra lateral
      br(),
      div(style = "padding: 15px;",
          h5("💡 Consejos", style = "color: #fff; font-weight: bold;"),
          p("• Configure Python antes de buscar", style = "color: #bbb; font-size: 12px;"),
          p("• Use fechas en formato YYYY-MM-DD", style = "color: #bbb; font-size: 12px;"),
          p("• Las coordenadas deben estar en WGS84", style = "color: #bbb; font-size: 12px;")
      )
    )
  ),

  # Cuerpo principal del dashboard
  dashboardBody(
    # Inicialización de shinyjs
    useShinyjs(),

    # Estilos CSS personalizados
    tags$head(
      tags$style(HTML("
        /* Tema general */
        .content-wrapper, .right-side {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }

        /* Cajas principales */
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

        /* Botones mejorados */
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

        /* Inputs mejorados */
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

        /* Labels mejorados */
        .control-label {
          font-weight: 600;
          color: #495057;
          margin-bottom: 8px;
          display: flex;
          align-items: center;
        }

        /* Miniaturas mejoradas */
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

        /* Mensajes de error mejorados */
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

        /* Info boxes mejoradas */
        .info-box {
          border-radius: 15px;
          margin-bottom: 20px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
        }

        .info-box:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.15);
        }

        /* Spinner personalizado */
        .spinner {
          border: 3px solid rgba(102, 126, 234, 0.3);
          border-top: 3px solid #667eea;
          border-radius: 50%;
          width: 40px;
          height: 40px;
          animation: spin 1s linear infinite;
          margin: 20px auto;
        }

        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }

        /* Contenedor de resultados */
        .results-container {
          background: rgba(255,255,255,0.95);
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        }

        /* Badges y etiquetas */
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

        .status-warning {
          background: linear-gradient(45deg, #f7971e, #ffd200);
          color: white;
        }

        .status-error {
          background: linear-gradient(45deg, #ff6b6b, #ee5a52);
          color: white;
        }

        /* Responsive design */
        @media (max-width: 768px) {
          .thumbnail {
            margin: 8px;
          }

          .thumbnail img {
            width: 120px;
            height: 80px;
          }

          .box {
            margin: 10px 5px;
          }
        }

        /* Animaciones de entrada */
        .fade-in {
          animation: fadeIn 0.5s ease-in;
        }

        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }

        /* Tooltip personalizado */
        .custom-tooltip {
          position: relative;
          cursor: help;
        }

        .custom-tooltip:hover::after {
          content: attr(data-tooltip);
          position: absolute;
          bottom: 100%;
          left: 50%;
          transform: translateX(-50%);
          background: rgba(0,0,0,0.9);
          color: white;
          padding: 8px 12px;
          border-radius: 8px;
          font-size: 12px;
          white-space: nowrap;
          z-index: 1000;
        }
      "))
    ),

    # Contenido de las pestañas
    tabItems(
      # ========================================================================
      # PESTAÑA: BÚSQUEDA Y DESCARGA
      # ========================================================================
      tabItem(tabName = "search",
              div(class = "fade-in",
                  # Fila 1: Credenciales y Configuración
                  fluidRow(
                    # Caja de credenciales
                    box(
                      title = "🔐 Credenciales HDA",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      div(
                        p("Ingrese sus credenciales para acceder a la API de HDA:",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        div(class = "form-group",
                            tags$label("👤 Usuario:", class = "control-label"),
                            textInput("user", "", placeholder = "1234")
                        ),
                        div(class = "form-group",
                            tags$label("🔑 Contraseña:", class = "control-label"),
                            passwordInput("password", "", placeholder = "Su contraseña segura")
                        ),
                        tags$small("Sus credenciales se mantienen seguras y no se almacenan.",
                                   style = "color: #28a745; font-style: italic;")
                      )
                    ),

                    # Caja de configuración
                    box(
                      title = "⚙️ Configuración del Sistema",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      div(
                        p("Configure las rutas necesarias para el funcionamiento:",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        div(class = "form-group",
                            tags$label("🐍 Ruta de Python:", class = "control-label"),
                            textInput("ruta_python", "",
                                      placeholder = "/usr/bin/python3 o C:/Python38/python.exe")
                        ),
                        div(class = "form-group",
                            tags$label("📁 Ruta de Descarga:", class = "control-label"),
                            textInput("download_path", "",
                                      placeholder = "/ruta/a/descargas o C:/Descargas")
                        ),
                        div(style = "text-align: center; margin-top: 20px;",
                            actionButton("check_config",
                                         "✅ Verificar Configuración",
                                         icon = icon("check"),
                                         class = "btn-success",
                                         style = "width: 100%;")
                        )
                      )
                    )
                  ),

                  # Fila 2: Parámetros de búsqueda
                  fluidRow(
                    box(
                      title = "🔍 Parámetros de Búsqueda Avanzada",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Configure los criterios para filtrar las imágenes satelitales:",
                          style = "color: #6c757d; margin-bottom: 25px;"),

                        # Primera fila de parámetros
                        fluidRow(
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📊 ID del Dataset:", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Identificador único del conjunto de datos"),
                                     textInput("dataset_id", "EO:EEA:DAT:CLMS_HRVPP_VI", placeholder = "EO:EEA:DAT:CLMS_HRVPP_VI")
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("🛰️ Tipo de Producto:", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Especifica el tipo específico de producto satelital"),
                                     textInput("productType", "NDVI-PPI-FAPAR-LAI-QFLAG2", placeholder = "NDVI")
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("🔢 ID de Plataforma:", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Identificador de la plataforma satelital"),
                                     textInput("platformSerialIdentifier", "", placeholder = "S2A")
                                 )
                          )
                        ),

                        # Segunda fila de parámetros
                        fluidRow(
                          column(4,
                                 div(class = "form-group",
                                     tags$label("🗂️ ID del Tile:", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Identificador específico del tile o azulejo"),
                                     textInput("tileId", "", placeholder = "30TYK")
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📅 Fecha de Inicio:", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Fecha de inicio en formato YYYY-MM-DD"),
                                     textInput("start", "", placeholder = "2023-01-01")
                                 )
                          ),
                          column(4,
                                 div(class = "form-group",
                                     tags$label("📅 Fecha de Fin:", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Fecha de fin en formato YYYY-MM-DD"),
                                     textInput("end", "", placeholder = "2023-12-31")
                                 )
                          )
                        ),

                        # Tercera fila: Bounding Box
                        fluidRow(
                          column(12,
                                 div(class = "form-group",
                                     tags$label("🗺️ Área de Interés (Bbox):", class = "control-label custom-tooltip",
                                                `data-tooltip` = "Coordenadas del área: longitud_min,latitud_min,longitud_max,latitud_max"),
                                     textInput("bbox", "",
                                               placeholder = "-3.7038,40.4168,-3.7002,40.4200 (Madrid, España)"),
                                     tags$small("Formato: xmin,ymin,xmax,ymax en coordenadas WGS84 (±180° lon, ±90° lat)",
                                                style = "color: #17a2b8; font-style: italic;")
                                 )
                          )
                        ),

                        # Área de mensajes de error
                        div(id = "errorMessages"),

                        # Botones de acción
                        div(style = "text-align: center; margin-top: 30px;",
                            fluidRow(
                              column(6,
                                     actionButton("search",
                                                  "🔍 Buscar Imágenes",
                                                  icon = icon("search"),
                                                  class = "btn-primary",
                                                  style = "width: 90%; margin-right: 5%;")
                              ),
                              column(6,
                                     actionButton("download",
                                                  "⬇️ Descargar Resultados",
                                                  icon = icon("download"),
                                                  class = "btn-success",
                                                  style = "width: 90%; margin-left: 5%;")
                              )
                            )
                        )
                      )
                    )
                  ),

                  # Fila 3: Resultados de búsqueda
                  fluidRow(
                    box(
                      title = "📋 Resultados de la Búsqueda",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(class = "results-container",
                          withSpinner(
                            verbatimTextOutput("result"),
                            type = 5,
                            color = "#667eea",
                            size = 0.8
                          )
                      )
                    )
                  )
              )
      ),

      # ========================================================================
      # PESTAÑA: VISUALIZACIÓN
      # ========================================================================
      tabItem(tabName = "visualization",
              div(class = "fade-in",
                  # Galería de imágenes
                  fluidRow(
                    box(
                      title = "🖼️ Galería de Imágenes Descargadas",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Haga clic en cualquier miniatura para ver la imagen en detalle:",
                          style = "color: #6c757d; margin-bottom: 20px; text-align: center;"),
                        withSpinner(
                          uiOutput("thumbnails"),
                          type = 6,
                          color = "#667eea"
                        )
                      )
                    )
                  ),

                  # Visualización detallada
                  fluidRow(
                    box(
                      title = "🔍 Visualización Detallada",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        withSpinner(
                          plotOutput("selected_image", height = "600px"),
                          type = 4,
                          color = "#667eea"
                        )
                      )
                    )
                  )
              )
      ),

      # ========================================================================
      # PESTAÑA: CONFIGURACIÓN Y ESTADO DEL SISTEMA
      # ========================================================================
      tabItem(tabName = "config",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📊 Estado del Sistema",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Información detallada sobre el estado actual del sistema:",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        div(class = "results-container",
                            withSpinner(
                              verbatimTextOutput("system_info"),
                              type = 7,
                              color = "#667eea"
                            )
                        )
                      )
                    )
                  ),

                  # Información adicional sobre la aplicación
                  fluidRow(
                    box(
                      title = "ℹ️ Información de la Aplicación",
                      status = "info",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        h4("📖 Acerca de DownloadVI", style = "color: #495057; margin-bottom: 15px;"),
                        p("DownloadVI es una aplicación Shiny desarrollada para facilitar la búsqueda,
                  descarga y visualización de imágenes satelitales a través de la API de HDA
                  (Harmonized Data Access).", style = "text-align: justify;"),

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
                          tags$li("R ≥ 4.0 con las librerías especificadas"),
                          tags$li("Python ≥ 3.6 con acceso a la librería HDA"),
                          tags$li("Credenciales válidas para HDA"),
                          tags$li("Conexión a internet estable")
                        ),

                        div(style = "text-align: center; margin-top: 30px; padding: 20px;
                           background: linear-gradient(45deg, #667eea, #764ba2);
                           border-radius: 15px; color: white;",
                            h5("💡 ¿Necesita ayuda?", style = "margin-bottom: 10px;"),
                            p("Consulte la documentación oficial de HDA o contacte al administrador del sistema.",
                              style = "margin: 0; font-style: italic;")
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
# LÓGICA DEL SERVIDOR
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # VALORES REACTIVOS
  # ==========================================================================
  # Almacena el estado de la aplicación y los datos compartidos entre funciones

  values <- reactiveValues(
    python_configured = FALSE,  # Estado de configuración de Python
    hda_client = NULL,         # Cliente HDA inicializado
    search_results = NULL,     # Resultados de la última búsqueda
    downloaded_files = NULL,   # Lista de archivos descargados
    selected_image = NULL      # Imagen seleccionada para visualización
  )

  # ==========================================================================
  # VERIFICACIÓN DE CONFIGURACIÓN
  # ==========================================================================
  # Maneja la verificación y configuración inicial del sistema

  observeEvent(input$check_config, {
    # Mostrar progreso de verificación
    withProgress(message = '🔧 Verificando configuración...', {

      # Paso 1: Verificar Python (33% del progreso)
      incProgress(1/3, detail = "Configurando Python...")
      python_ok <- configurar_python(input$ruta_python)

      if (python_ok) {
        values$python_configured <- TRUE
        showNotification(
          "✅ Python configurado correctamente",
          type = "message",
          duration = 5
        )
      } else {
        showNotification(
          "❌ Error al configurar Python. Verifique la ruta.",
          type = "error",
          duration = 8
        )
        return()
      }

      # Paso 2: Verificar ruta de descarga (66% del progreso)
      incProgress(1/3, detail = "Verificando ruta de descarga...")
      download_path <- input$download_path

      if (dir.exists(download_path)) {
        showNotification(
          "✅ Ruta de descarga verificada",
          type = "message",
          duration = 5
        )
      } else {
        # Intentar crear el directorio
        dir_created <- dir.create(download_path, recursive = TRUE, showWarnings = FALSE)
        if (dir_created) {
          showNotification(
            "✅ Ruta de descarga creada exitosamente",
            type = "message",
            duration = 5
          )
        } else {
          showNotification(
            "❌ No se pudo crear la ruta de descarga",
            type = "error",
            duration = 8
          )
        }
      }

      # Paso 3: Inicializar cliente HDA (100% del progreso)
      incProgress(1/3, detail = "Conectando con HDA...")
      tryCatch({
        hda <- import("hda")
        conf <- hda$Configuration(user = input$user, password = input$password)
        values$hda_client <- hda$Client(config = conf)
        showNotification(
          "🌐 Conexión con HDA establecida correctamente",
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("❌ Error al conectar con HDA:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # ==========================================================================
  # BÚSQUEDA DE IMÁGENES
  # ==========================================================================
  # Maneja la búsqueda de imágenes satelitales con validación completa

  observeEvent(input$search, {
    # Limpiar mensajes de error previos
    html("errorMessages", "")

    # Validar todas las entradas
    errors <- validate_inputs(input)
    if (length(errors) > 0) {
      error_html <- paste(
        "<div class='alert alert-danger' role='alert'>",
        "<h5>⚠️ Errores de validación:</h5>",
        "<ul>",
        paste("<li>", errors, "</li>", collapse = ""),
        "</ul>",
        "</div>"
      )
      html("errorMessages", error_html)
      return()
    }

    # Verificar configuración previa
    if (!values$python_configured) {
      showNotification(
        "⚙️ Python no configurado. Por favor verifique la configuración primero.",
        type = "error",
        duration = 8
      )
      return()
    }

    if (is.null(values$hda_client)) {
      showNotification(
        "🔌 Cliente HDA no inicializado. Por favor verifique la configuración primero.",
        type = "error",
        duration = 8
      )
      return()
    }

    # Preparar parámetros de consulta
    query <- list(dataset_id = input$dataset_id)

    # Agregar parámetros opcionales si están presentes
    if (input$productType != "") query$productType <- input$productType
    if (input$platformSerialIdentifier != "") query$platformSerialIdentifier <- input$platformSerialIdentifier
    if (input$tileId != "") query$tileId <- input$tileId
    if (input$start != "") query$start <- input$start
    if (input$end != "") query$end <- input$end

    # Agregar bbox si está presente
    if (input$bbox != "") {
      query$bbox <- as.numeric(unlist(strsplit(input$bbox, ",")))
    }

    # Ejecutar búsqueda con indicador de progreso
    withProgress(message = '🔍 Buscando imágenes...', {
      incProgress(0.5, detail = "Procesando consulta...")

      tryCatch({
        values$search_results <- values$hda_client$search(query)

        incProgress(0.5, detail = "Finalizando búsqueda...")

        if (length(values$search_results) > 0) {
          showNotification(
            paste("🎉", length(values$search_results), "imágenes encontradas exitosamente"),
            type = "message",
            duration = 6
          )
        } else {
          showNotification(
            "⚠️ No se encontraron imágenes con los criterios especificados",
            type = "warning",
            duration = 8
          )
        }
      }, error = function(e) {
        showNotification(
          paste("❌ Error en la búsqueda:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # ==========================================================================
  # MOSTRAR RESULTADOS DE BÚSQUEDA
  # ==========================================================================
  # Renderiza los resultados de búsqueda de forma amigable

  output$result <- renderPrint({
    if (!is.null(values$search_results)) {
      cat("📊 RESULTADOS DE LA BÚSQUEDA\n")
      cat("═══════════════════════════════\n\n")
      cat("🔢 Total de imágenes encontradas:", length(values$search_results), "\n\n")

      if (length(values$search_results) > 0) {
        cat("📋 Detalles de los primeros 5 resultados:\n")
        cat("─────────────────────────────────────────\n")

        # Mostrar información de hasta 5 resultados
        max_show <- min(5, length(values$search_results))
        for (i in 1:max_show) {
          cat(sprintf("🖼️  Imagen %d:\n", i))
          cat(sprintf("   📅 Fecha: %s\n",
                      ifelse(is.null(values$search_results[[i]]$date), "N/A", values$search_results[[i]]$date)))
          cat(sprintf("   🆔 ID: %s\n",
                      ifelse(is.null(values$search_results[[i]]$id), "N/A", values$search_results[[i]]$id)))
          cat(sprintf("   📏 Tamaño: %s\n",
                      ifelse(is.null(values$search_results[[i]]$size), "N/A", values$search_results[[i]]$size)))
          cat("\n")
        }

        if (length(values$search_results) > 5) {
          cat(sprintf("... y %d imágenes más.\n\n", length(values$search_results) - 5))
        }

        cat("💡 Consejo: Use el botón 'Descargar' para obtener todas las imágenes.\n")
      }

      cat("\n⏰ Búsqueda realizada:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

    } else {
      cat("📭 No hay resultados para mostrar.\n")
      cat("═══════════════════════════════\n\n")
      cat("🔍 Para comenzar:\n")
      cat("   1. Configure Python y las credenciales\n")
      cat("   2. Complete los parámetros de búsqueda\n")
      cat("   3. Haga clic en 'Buscar Imágenes'\n\n")
      cat("💡 Asegúrese de que todos los campos obligatorios estén completos.")
    }
  })

  # ==========================================================================
  # DESCARGA DE IMÁGENES
  # ==========================================================================
  # Maneja la descarga de imágenes con seguimiento detallado del progreso

  observeEvent(input$download, {
    # Verificar que hay resultados para descargar
    if (is.null(values$search_results) || length(values$search_results) == 0) {
      showNotification(
        "⚠️ No hay resultados para descargar. Realice una búsqueda primero.",
        type = "warning",
        duration = 8
      )
      return()
    }

    download_path <- input$download_path

    # Crear directorio si no existe
    if (!dir.exists(download_path)) {
      dir.create(download_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Descarga con seguimiento detallado del progreso
    withProgress(message = '⬇️ Descargando imágenes...', {
      total_files <- length(values$search_results)
      downloaded_files <- character()
      failed_downloads <- 0

      for (i in seq_along(values$search_results)) {
        # Actualizar progreso
        progress_pct <- i / total_files
        incProgress(1/total_files,
                    detail = sprintf("Descargando %d de %d (%.1f%%)",
                                     i, total_files, progress_pct * 100))

        tryCatch({
          # Simular descarga (adaptar según la API real de HDA)
          file_path <- values$search_results[[i]]$download(download_path)
          downloaded_files <- c(downloaded_files, file_path)

          # Pausa pequeña para mostrar progreso
          Sys.sleep(0.1)

        }, error = function(e) {
          failed_downloads <<- failed_downloads + 1
          message(sprintf("Error descargando imagen %d: %s", i, e$message))
        })
      }

      # Actualizar lista de archivos descargados
      values$downloaded_files <- list.files(download_path,
                                            pattern = "\\.(tif|jpg|png)$",
                                            full.names = TRUE)

      # Mostrar resultado final
      if (length(values$downloaded_files) > 0) {
        success_msg <- sprintf("✅ %d imágenes descargadas correctamente",
                               length(values$downloaded_files))
        if (failed_downloads > 0) {
          success_msg <- paste(success_msg, sprintf("(%d fallaron)", failed_downloads))
        }
        showNotification(success_msg, type = "message", duration = 8)
      } else {
        showNotification("❌ No se pudieron descargar las imágenes",
                         type = "error", duration = 10)
      }
    })
  })

  # ==========================================================================
  # VISUALIZACIÓN DE MINIATURAS
  # ==========================================================================
  # Genera una galería interactiva de miniaturas de las imágenes descargadas

  output$thumbnails <- renderUI({
    if (!is.null(values$downloaded_files) && length(values$downloaded_files) > 0) {

      # Crear miniaturas para cada archivo
      thumbnails <- lapply(values$downloaded_files, function(file) {
        # Crear miniatura de forma segura
        thumbnail <- tryCatch({
          img <- image_read(file)
          img_thumbnail <- image_scale(img, "200x150!")  # Tamaño fijo
          thumbnail_path <- file.path(tempdir(),
                                      paste0("thumb_",
                                             tools::file_path_sans_ext(basename(file)),
                                             ".png"))
          image_write(img_thumbnail, path = thumbnail_path, format = "png")
          thumbnail_path
        }, error = function(e) {
          message("Error creando miniatura para ", basename(file), ": ", e$message)
          return(NULL)
        })

        filename <- basename(file)
        file_size <- file.size(file)
        formatted_size <- ifelse(file_size > 1024^2,
                                 paste(round(file_size / 1024^2, 1), "MB"),
                                 paste(round(file_size / 1024, 1), "KB"))

        if (!is.null(thumbnail)) {
          # Miniatura exitosa
          div(class = "thumbnail",
              onclick = paste0("Shiny.setInputValue('selected_file', '", file, "')"),
              style = "cursor: pointer;",
              tags$img(src = thumbnail,
                       alt = filename,
                       style = "width: 180px; height: 120px; object-fit: cover;"),
              div(class = "thumbnail-caption",
                  tags$strong(tools::file_path_sans_ext(filename)),
                  br(),
                  tags$small(formatted_size, style = "color: #6c757d;"),
                  br(),
                  span(class = "status-badge status-success", "✓ Listo")
              )
          )
        } else {
          # Error en miniatura
          div(class = "thumbnail",
              style = "cursor: pointer; opacity: 0.7;",
              onclick = paste0("Shiny.setInputValue('selected_file', '", file, "')"),
              div(style = "width: 180px; height: 120px; background: linear-gradient(45deg, #f8d7da, #f5c6cb);
                          display: flex; align-items: center; justify-content: center;
                          border-radius: 10px; color: #721c24;",
                  tags$div(style = "text-align: center;",
                           tags$i(class = "fa fa-exclamation-triangle",
                                  style = "font-size: 24px; margin-bottom: 5px;"),
                           br(),
                           "Error en miniatura"
                  )
              ),
              div(class = "thumbnail-caption",
                  tags$strong(tools::file_path_sans_ext(filename)),
                  br(),
                  tags$small(formatted_size, style = "color: #6c757d;"),
                  br(),
                  span(class = "status-badge status-error", "⚠ Error")
              )
          )
        }
      })

      # Contenedor de la galería
      div(class = "gallery-container",
          style = "text-align: center; padding: 20px;",
          div(style = "margin-bottom: 20px;",
              h4("📸 Galería de Imágenes", style = "color: #495057;"),
              p(sprintf("Total: %d imágenes descargadas", length(values$downloaded_files)),
                style = "color: #6c757d;")
          ),
          do.call(tagList, thumbnails),
          div(style = "margin-top: 30px; padding: 20px;
                     background: rgba(255,255,255,0.8); border-radius: 15px;",
              p("💡 Consejo: Haga clic en cualquier miniatura para ver la imagen completa en el panel inferior.",
                style = "color: #17a2b8; font-style: italic; margin: 0;")
          )
      )

    } else {
      # Sin imágenes descargadas
      div(style = "text-align: center; padding: 60px 20px;",
          div(style = "background: rgba(255,255,255,0.9); border-radius: 20px;
                     padding: 40px; box-shadow: 0 4px 20px rgba(0,0,0,0.1);",
              tags$i(class = "fa fa-images",
                     style = "font-size: 48px; color: #6c757d; margin-bottom: 20px;"),
              h4("📭 No hay imágenes para mostrar", style = "color: #495057;"),
              p("Para ver imágenes aquí:", style = "color: #6c757d; margin-top: 20px;"),
              tags$ol(style = "text-align: left; display: inline-block; color: #6c757d;",
                      tags$li("Configure las credenciales y rutas"),
                      tags$li("Realice una búsqueda de imágenes"),
                      tags$li("Descargue los resultados"),
                      tags$li("Las miniaturas aparecerán automáticamente")
              )
          )
      )
    }
  })

  # ==========================================================================
  # MANEJO DE SELECCIÓN DE MINIATURAS
  # ==========================================================================
  # Detecta cuando el usuario selecciona una miniatura

  observeEvent(input$selected_file, {
    values$selected_image <- input$selected_file
    showNotification(
      paste("🖼️ Imagen seleccionada:", basename(input$selected_file)),
      type = "message",
      duration = 3
    )
  })

  # ==========================================================================
  # VISUALIZACIÓN DE IMAGEN SELECCIONADA
  # ==========================================================================
  # Renderiza la imagen seleccionada en tamaño completo

  output$selected_image <- renderPlot({
    if (!is.null(values$selected_image) && file.exists(values$selected_image)) {
      tryCatch({
        # Cargar y mostrar la imagen
        img <- image_read(values$selected_image)

        # Crear el plot
        par(mar = c(2, 2, 3, 2), bg = "white")
        plot(as.raster(img),
             main = paste("📸", basename(values$selected_image)),
             cex.main = 1.2,
             col.main = "#495057")

        # Agregar información adicional
        file_info <- file.info(values$selected_image)
        info_text <- sprintf("📊 Tamaño: %.1f MB | 📅 Modificado: %s",
                             file_info$size / 1024^2,
                             format(file_info$mtime, "%Y-%m-%d %H:%M"))

        mtext(info_text, side = 1, line = 0.5, cex = 0.8, col = "#6c757d")

      }, error = function(e) {
        # Plot de error
        par(mar = c(4, 4, 4, 4), bg = "#f8f9fa")
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
             main = "❌ Error al cargar la imagen",
             col.main = "#dc3545", cex.main = 1.3,
             axes = FALSE)

        # Mensaje de error centrado
        text(0.5, 0.6, "No se pudo cargar la imagen seleccionada",
             col = "#dc3545", cex = 1.1, font = 2)
        text(0.5, 0.4, paste("Error:", e$message),
             col = "#6c757d", cex = 0.9)
        text(0.5, 0.2, "Intente seleccionar otra imagen",
             col = "#17a2b8", cex = 0.9, font = 3)

        # Borde decorativo
        rect(0.1, 0.1, 0.9, 0.9, border = "#dee2e6", lwd = 2)
      })
    } else {
      # Placeholder cuando no hay imagen seleccionada
      par(mar = c(4, 4, 4, 4), bg = "#f8f9fa")
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
           main = "🖼️ Visualización de Imágenes",
           col.main = "#495057", cex.main = 1.4,
           axes = FALSE)

      # Instrucciones centradas
      text(0.5, 0.7, "Seleccione una imagen para visualizar",
           col = "#495057", cex = 1.2, font = 2)
      text(0.5, 0.5, "👆 Haga clic en una miniatura",
           col = "#6c757d", cex = 1.1)
      text(0.5, 0.3, "de la galería superior",
           col = "#6c757d", cex = 1.1)

      # Ícono decorativo
      points(0.5, 0.15, pch = 16, cex = 8, col = "#e9ecef")
      text(0.5, 0.15, "📷", cex = 3)

      # Borde decorativo
      rect(0.05, 0.05, 0.95, 0.95, border = "#dee2e6", lwd = 2, lty = 2)
    }
  })

  # ==========================================================================
  # INFORMACIÓN DEL SISTEMA
  # ==========================================================================
  # Muestra información detallada sobre el estado del sistema

  output$system_info <- renderPrint({
    cat("🖥️  ESTADO DEL SISTEMA - DOWNLOADVI\n")
    cat("═══════════════════════════════════════════\n\n")

    # Configuración básica
    cat("⚙️  CONFIGURACIÓN BÁSICA:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("🐍 Python configurado: %s\n",
                ifelse(values$python_configured, "✅ Sí", "❌ No")))
    cat(sprintf("🔌 Cliente HDA: %s\n",
                ifelse(!is.null(values$hda_client), "✅ Conectado", "❌ No conectado")))
    cat(sprintf("👤 Usuario: %s\n",
                ifelse(input$user != "", input$user, "No especificado")))
    cat(sprintf("🐍 Ruta Python: %s\n",
                ifelse(input$ruta_python != "", input$ruta_python, "No especificada")))
    cat(sprintf("📁 Ruta descarga: %s\n",
                ifelse(input$download_path != "", input$download_path, "No especificada")))
    cat(sprintf("📂 Directorio existe: %s\n",
                ifelse(dir.exists(input$download_path), "✅ Sí", "❌ No")))

    cat("\n📊 ESTADÍSTICAS DE DATOS:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("🔍 Imágenes encontradas: %d\n",
                ifelse(!is.null(values$search_results), length(values$search_results), 0)))
    cat(sprintf("⬇️  Imágenes descargadas: %d\n",
                ifelse(!is.null(values$downloaded_files), length(values$downloaded_files), 0)))
    cat(sprintf("🖼️  Imagen seleccionada: %s\n",
                ifelse(!is.null(values$selected_image), basename(values$selected_image), "Ninguna")))

    # Información del sistema
    cat("\n🔧 INFORMACIÓN DEL SISTEMA:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("📅 Fecha actual: %s\n", format(Sys.Date(), "%Y-%m-%d")))
    cat(sprintf("⏰ Hora actual: %s\n", format(Sys.time(), "%H:%M:%S")))
    cat(sprintf("💾 Directorio temporal: %s\n", tempdir()))
    cat(sprintf("👥 Usuario del sistema: %s\n", Sys.info()["user"]))
    cat(sprintf("🖥️  Sistema operativo: %s\n", Sys.info()["sysname"]))

    # Versiones de paquetes
    cat("\n📦 VERSIONES DE PAQUETES:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("🔵 R: %s\n", R.version$version.string))
    cat(sprintf("✨ Shiny: %s\n", as.character(packageVersion("shiny"))))
    cat(sprintf("🐍 Reticulate: %s\n", as.character(packageVersion("reticulate"))))
    cat(sprintf("🖼️  Magick: %s\n", as.character(packageVersion("magick"))))
    cat(sprintf("📊 shinydashboard: %s\n", as.character(packageVersion("shinydashboard"))))
    cat(sprintf("⚡ shinyjs: %s\n", as.character(packageVersion("shinyjs"))))

    # Estado de memoria
    cat("\n💾 USO DE MEMORIA:\n")
    cat("─────────────────────────────\n")
    memory_info <- gc()
    cat(sprintf("🔋 Memoria usada: %.1f MB\n", sum(memory_info[,2])))
    cat(sprintf("🔄 Colecciones GC: %d\n", sum(memory_info[,4])))

    # Resumen final
    cat("\n📋 RESUMEN DEL ESTADO:\n")
    cat("─────────────────────────────\n")
    system_status <- "✅ OPERATIVO"
    if (!values$python_configured) system_status <- "⚠️  CONFIGURACIÓN PENDIENTE"
    if (is.null(values$hda_client)) system_status <- "❌ DESCONECTADO"

    cat(sprintf("🚥 Estado general: %s\n", system_status))
    cat(sprintf("⏱️  Última actualización: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

    cat("\n" , rep("═", 40), "\n")
    cat("💡 Sugerencia: Mantenga todas las configuraciones actualizadas\n")
    cat("   para un funcionamiento óptimo de la aplicación.\n")
  })
}

# ============================================================================
# INICIALIZACIÓN DE LA APLICACIÓN
# ============================================================================

# Crear y ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
