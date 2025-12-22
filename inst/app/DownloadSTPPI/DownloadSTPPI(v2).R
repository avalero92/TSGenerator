# ============================================================================
# Download_STPPI_App - Interfaz Shiny idéntica a DownloadVI
# Con selector de carpetas que muestra todos los discos locales
# ============================================================================
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyFiles)
library(reticulate)
library(magick)
library(fs)
library(rlang)  # Para %||%

# ============================================================================
# UI
# ============================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-satellite", style = "margin-right: 8px;"),
      "Download STPPI",
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
      # ========================================================================
      # PESTAÑA: BÚSQUEDA Y DESCARGA
      # ========================================================================
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
                          p("Configure los criterios para filtrar los productos STPPI:", style = "color: #6c757d; margin-bottom: 25px;"),
                          fluidRow(
                            column(4,
                                   div(class = "form-group",
                                       tags$label("📊 ID del Dataset:", class = "control-label"),
                                       textInput("dataset_id", "", placeholder = "Ej: EO:EEA:DAT:CLMS_STPPI")
                                   )
                            ),
                            column(4,
                                   div(class = "form-group",
                                       tags$label("🛰️ Tipo de Producto:", class = "control-label"),
                                       textInput("productType", "", placeholder = "Ej: STPPI_300m")
                                   )
                            ),
                            column(4,
                                   div(class = "form-group",
                                       tags$label("🔢 Plataforma:", class = "control-label"),
                                       textInput("platformSerialIdentifier", "", placeholder = "Ej: S2A")
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

      # ========================================================================
      # PESTAÑA: VISUALIZACIÓN
      # ========================================================================
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

      # ========================================================================
      # PESTAÑA: CONFIGURACIÓN Y ESTADO
      # ========================================================================
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
                          h4("📖 Acerca de Download STPPI", style = "color: #495057; margin-bottom: 15px;"),
                          p("Aplicación Shiny para búsqueda, descarga y visualización de productos STPPI a través de la API HDA.", style = "text-align: justify;"),
                          h5("🚀 Características principales:", style = "color: #495057; margin-top: 20px;"),
                          tags$ul(
                            tags$li("✅ Interfaz intuitiva y moderna"),
                            tags$li("🔒 Gestión segura de credenciales"),
                            tags$li("🔍 Búsqueda avanzada con múltiples filtros"),
                            tags$li("📊 Validación robusta de entrada"),
                            tags$li("⬇️ Descarga automática con progreso"),
                            tags$li("🖼️ Visualización interactiva de imágenes"),
                            tags$li("📱 Diseño completamente responsivo")
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
  # DETECCIÓN DE TODOS LOS DISCOS LOCALES
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

  shinyDirChoose(input, "download_dir", roots = volumes, allowDirCreate = TRUE)

  download_path <- reactive({
    req(input$download_dir)
    parseDirPath(roots = volumes, input$download_dir)
  })

  output$download_path_display <- renderPrint({
    if (is.null(input$download_dir)) {
      cat("Ninguna carpeta seleccionada")
    } else {
      cat(download_path())
    }
  })

  # ==========================================================================
  # VALORES REACTIVOS
  # ==========================================================================
  values <- reactiveValues(
    python_configured = FALSE,
    hda_client = NULL,
    search_results = NULL,
    downloaded_files = NULL,
    selected_image = NULL
  )

  # ==========================================================================
  # VERIFICACIÓN DE CONFIGURACIÓN
  # ==========================================================================
  observeEvent(input$check_config, {
    withProgress(message = '🔧 Verificando configuración...', {
      incProgress(1/3, detail = "Configurando Python...")
      python_ok <- tryCatch({
        if (file.exists(input$ruta_python)) {
          use_python(input$ruta_python, required = TRUE)
          TRUE
        } else FALSE
      }, error = function(e) FALSE)

      values$python_configured <- python_ok
      if (python_ok) {
        showNotification("✅ Python configurado correctamente", type = "message", duration = 5)
      } else {
        showNotification("❌ Error al configurar Python. Verifique la ruta.", type = "error", duration = 8)
        return()
      }

      incProgress(1/3, detail = "Verificando carpeta descarga...")
      if (!is.null(download_path())) {
        dir.create(download_path(), recursive = TRUE, showWarnings = FALSE)
        showNotification("✅ Carpeta de descarga lista", type = "message", duration = 5)
      }

      incProgress(1/3, detail = "Conectando con HDA...")
      tryCatch({
        hda <- import("hda")
        conf <- hda$Configuration(user = input$user, password = input$password)
        values$hda_client <- hda$Client(config = conf)
        showNotification("🌐 Conexión con HDA establecida correctamente", type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("❌ Error al conectar con HDA:", e$message), type = "error", duration = 10)
      })
    })
  })

  # ==========================================================================
  # BÚSQUEDA
  # ==========================================================================
  observeEvent(input$search, {
    html("errorMessages", "")
    errors <- character()
    if (input$user == "") errors <- c(errors, "📧 Usuario es requerido")
    if (input$password == "") errors <- c(errors, "🔐 Contraseña es requerida")
    if (input$dataset_id == "") errors <- c(errors, "📊 ID del Dataset es requerido")
    if (!values$python_configured) errors <- c(errors, "🐍 Configure Python primero")

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

    withProgress(message = '🔍 Buscando productos STPPI...', {
      incProgress(0.5, detail = "Procesando consulta...")
      tryCatch({
        query <- list(
          dataset_id = input$dataset_id
        )
        if (input$productType != "") query$productType <- input$productType
        if (input$platformSerialIdentifier != "") query$platformSerialIdentifier <- input$platformSerialIdentifier
        if (input$tileId != "") query$tileId <- input$tileId
        if (input$start != "") query$start <- input$start
        if (input$end != "") query$end <- input$end
        if (input$bbox != "") {
          bbox_vals <- as.numeric(unlist(strsplit(input$bbox, ",")))
          if (length(bbox_vals) == 4) query$bbox <- bbox_vals
        }

        values$search_results <- values$hda_client$search(query)
        incProgress(0.5, detail = "Finalizando búsqueda...")
        if (py_len(values$search_results) > 0) {
          showNotification(paste("🎉", py_len(values$search_results), "productos encontrados"), type = "message", duration = 6)
        } else {
          showNotification("⚠️ No se encontraron productos con los criterios especificados", type = "warning", duration = 8)
        }
      }, error = function(e) {
        showNotification(paste("❌ Error en la búsqueda:", e$message), type = "error", duration = 10)
      })
    })
  })

  # ==========================================================================
  # RESULTADOS DE BÚSQUEDA (CORREGIDO PARA hda)
  # ==========================================================================
  output$result <- renderPrint({
    if (!is.null(values$search_results)) {
      total <- py_len(values$search_results)
      cat("📊 RESULTADOS DE LA BÚSQUEDA\n")
      cat("═══════════════════════════════\n\n")
      cat("🔢 Total de productos encontrados:", total, "\n\n")
      if (total > 0) {
        cat("📋 Detalles de los primeros 5 resultados:\n")
        cat("─────────────────────────────────────────\n")
        max_show <- min(5, total)
        for (i in 1:max_show) {
          item <- values$search_results[[i]]
          props <- item$properties %||% py_dict()
          cat(sprintf("🖼️ Producto %d:\n", i))
          cat(sprintf(" 🆔 ID: %s\n", item$id %||% "N/A"))
          cat(sprintf(" 🏷️ Título: %s\n", props$title %||% "N/A"))
          cat(sprintf(" 📅 Fecha: %s\n", props$date %||% props$startDate %||% "N/A"))
          cat(sprintf(" 📏 Tamaño: %s\n\n", props$size %||% "N/A"))
        }
        if (total > 5) {
          cat(sprintf("... y %d productos más.\n\n", total - 5))
        }
        cat("💡 Consejo: Use el botón 'Descargar Resultados' para descargar todos.\n")
      }
      cat("\n⏰ Búsqueda realizada:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    } else {
      cat("📭 No hay resultados para mostrar.\n")
      cat("═══════════════════════════════\n\n")
      cat("🔍 Para comenzar:\n")
      cat(" 1. Configure Python y las credenciales\n")
      cat(" 2. Complete los parámetros de búsqueda\n")
      cat(" 3. Haga clic en 'Buscar Productos'\n")
    }
  })

  # ==========================================================================
  # DESCARGA (MÉTODO NATIVO DE hda - MÁS EFICIENTE)
  # ==========================================================================
  observeEvent(input$download, {
    if (is.null(values$search_results) || py_len(values$search_results) == 0) {
      showNotification("⚠️ No hay resultados para descargar. Realice una búsqueda primero.", type = "warning", duration = 8)
      return()
    }
    if (is.null(download_path())) {
      showNotification("📁 Seleccione una carpeta de descarga", type = "error", duration = 8)
      return()
    }

    dir.create(download_path(), recursive = TRUE, showWarnings = FALSE)

    withProgress(message = '⬇️ Descargando todos los productos...', value = 0, {
      tryCatch({
        values$search_results$download(download_path())
        incProgress(1)
        values$downloaded_files <- list.files(download_path(), pattern = "\\.(tif|zip|jpg|png)$", full.names = TRUE, ignore.case = TRUE)
        showNotification(paste("✅ Descarga completada:", length(values$downloaded_files), "archivos"), type = "message", duration = 8)
      }, error = function(e) {
        showNotification(paste("❌ Error durante la descarga:", e$message), type = "error", duration = 10)
      })
    })
  })

  # ==========================================================================
  # GALERÍA DE MINIATURAS
  # ==========================================================================
  output$thumbnails <- renderUI({
    if (!is.null(values$downloaded_files) && length(values$downloaded_files) > 0) {
      thumbnails <- lapply(values$downloaded_files, function(file) {
        thumbnail <- tryCatch({
          img <- image_read(file)
          img_thumbnail <- image_scale(img, "200x150!")
          thumbnail_path <- file.path(tempdir(), paste0("thumb_", tools::file_path_sans_ext(basename(file)), ".png"))
          image_write(img_thumbnail, path = thumbnail_path, format = "png")
          thumbnail_path
        }, error = function(e) NULL)

        filename <- basename(file)
        file_size <- file.size(file)
        formatted_size <- ifelse(file_size > 1024^2, paste(round(file_size / 1024^2, 1), "MB"), paste(round(file_size / 1024, 1), "KB"))

        if (!is.null(thumbnail)) {
          div(class = "thumbnail",
              onclick = paste0("Shiny.setInputValue('selected_file', '", file, "')"),
              style = "cursor: pointer;",
              tags$img(src = thumbnail, alt = filename, style = "width: 180px; height: 120px; object-fit: cover;"),
              div(class = "thumbnail-caption",
                  tags$strong(tools::file_path_sans_ext(filename)),
                  br(),
                  tags$small(formatted_size, style = "color: #6c757d;"),
                  br(),
                  span(class = "status-badge status-success", "✓ Listo")
              )
          )
        } else {
          div(class = "thumbnail",
              style = "cursor: pointer; opacity: 0.7;",
              onclick = paste0("Shiny.setInputValue('selected_file', '", file, "')"),
              div(style = "width: 180px; height: 120px; background: linear-gradient(45deg, #f8d7da, #f5c6cb); display: flex; align-items: center; justify-content: center; border-radius: 10px; color: #721c24;",
                  tags$div(style = "text-align: center;",
                           tags$i(class = "fa fa-exclamation-triangle", style = "font-size: 24px; margin-bottom: 5px;"),
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

      div(class = "gallery-container",
          style = "text-align: center; padding: 20px;",
          div(style = "margin-bottom: 20px;",
              h4("📸 Galería de Productos STPPI", style = "color: #495057;"),
              p(sprintf("Total: %d archivos descargados", length(values$downloaded_files)), style = "color: #6c757d;")
          ),
          do.call(tagList, thumbnails),
          div(style = "margin-top: 30px; padding: 20px; background: rgba(255,255,255,0.8); border-radius: 15px;",
              p("💡 Consejo: Haga clic en cualquier miniatura para ver el archivo en detalle.", style = "color: #17a2b8; font-style: italic; margin: 0;")
          )
      )
    } else {
      div(style = "text-align: center; padding: 60px 20px;",
          div(style = "background: rgba(255,255,255,0.9); border-radius: 20px; padding: 40px; box-shadow: 0 4px 20px rgba(0,0,0,0.1);",
              tags$i(class = "fa fa-images", style = "font-size: 48px; color: #6c757d; margin-bottom: 20px;"),
              h4("📭 No hay archivos para mostrar", style = "color: #495057;"),
              p("Para ver contenido aquí:", style = "color: #6c757d; margin-top: 20px;"),
              tags$ol(style = "text-align: left; display: inline-block; color: #6c757d;",
                      tags$li("Configure las credenciales y rutas"),
                      tags$li("Realice una búsqueda"),
                      tags$li("Descargue los resultados"),
                      tags$li("Los archivos aparecerán automáticamente")
              )
          )
      )
    }
  })

  observeEvent(input$selected_file, {
    values$selected_image <- input$selected_file
    showNotification(paste("🖼️ Archivo seleccionado:", basename(input$selected_file)), type = "message", duration = 3)
  })

  output$selected_image <- renderPlot({
    if (!is.null(values$selected_image) && file.exists(values$selected_image)) {
      tryCatch({
        img <- image_read(values$selected_image)
        par(mar = c(2, 2, 3, 2), bg = "white")
        plot(as.raster(img),
             main = paste("📸", basename(values$selected_image)),
             cex.main = 1.2,
             col.main = "#495057")
        file_info <- file.info(values$selected_image)
        info_text <- sprintf("📊 Tamaño: %.1f MB | 📅 Modificado: %s",
                             file_info$size / 1024^2,
                             format(file_info$mtime, "%Y-%m-%d %H:%M"))
        mtext(info_text, side = 1, line = 0.5, cex = 0.8, col = "#6c757d")
      }, error = function(e) {
        par(mar = c(4, 4, 4, 4), bg = "#f8f9fa")
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
             main = "❌ Error al cargar el archivo",
             col.main = "#dc3545", cex.main = 1.3,
             axes = FALSE)
        text(0.5, 0.6, "No se pudo cargar el archivo seleccionado", col = "#dc3545", cex = 1.1, font = 2)
        text(0.5, 0.4, paste("Error:", e$message), col = "#6c757d", cex = 0.9)
        text(0.5, 0.2, "Intente seleccionar otro archivo", col = "#17a2b8", cex = 0.9, font = 3)
        rect(0.1, 0.1, 0.9, 0.9, border = "#dee2e6", lwd = 2)
      })
    } else {
      par(mar = c(4, 4, 4, 4), bg = "#f8f9fa")
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
           main = "🖼️ Visualización de Productos",
           col.main = "#495057", cex.main = 1.4,
           axes = FALSE)
      text(0.5, 0.7, "Seleccione un archivo para visualizar", col = "#495057", cex = 1.2, font = 2)
      text(0.5, 0.5, "👆 Haga clic en una miniatura", col = "#6c757d", cex = 1.1)
      text(0.5, 0.3, "de la galería superior", col = "#6c757d", cex = 1.1)
      points(0.5, 0.15, pch = 16, cex = 8, col = "#e9ecef")
      text(0.5, 0.15, "📷", cex = 3)
      rect(0.05, 0.05, 0.95, 0.95, border = "#dee2e6", lwd = 2, lty = 2)
    }
  })

  # ==========================================================================
  # ESTADO DEL SISTEMA
  # ==========================================================================
  output$system_info <- renderPrint({
    cat("🖥️ ESTADO DEL SISTEMA - DOWNLOAD STPPI\n")
    cat("═══════════════════════════════════════════\n\n")
    cat("⚙️ CONFIGURACIÓN BÁSICA:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("🐍 Python configurado: %s\n", ifelse(values$python_configured, "✅ Sí", "❌ No")))
    cat(sprintf("🔌 Cliente HDA: %s\n", ifelse(!is.null(values$hda_client), "✅ Conectado", "❌ No conectado")))
    cat(sprintf("👤 Usuario: %s\n", ifelse(input$user != "", input$user, "No especificado")))
    cat(sprintf("🐍 Ruta Python: %s\n", ifelse(input$ruta_python != "", input$ruta_python, "No especificada")))
    cat(sprintf("📁 Ruta descarga: %s\n", ifelse(!is.null(download_path()), download_path(), "No seleccionada")))
    cat("\n📊 ESTADÍSTICAS DE DATOS:\n")
    cat("─────────────────────────────\n")
    total_found <- ifelse(!is.null(values$search_results), py_len(values$search_results), 0)
    cat(sprintf("🔍 Productos encontrados: %d\n", total_found))
    cat(sprintf("⬇️ Archivos descargados: %d\n", ifelse(!is.null(values$downloaded_files), length(values$downloaded_files), 0)))
    cat(sprintf("🖼️ Archivo seleccionado: %s\n", ifelse(!is.null(values$selected_image), basename(values$selected_image), "Ninguno")))
    cat("\n⏱️ Última actualización: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  })
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================
shinyApp(ui = ui, server = server)
