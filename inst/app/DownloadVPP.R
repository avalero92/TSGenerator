library(shiny)
library(reticulate)
library(magick)  # Para manejo de imágenes
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para funcionalidad JavaScript mejorada
library(shinycssloaders)  # Para indicadores de carga

# Función para configurar Python
configurar_python <- function(ruta_python) {
  tryCatch({
    if (file.exists(ruta_python)) {
      use_python(ruta_python, required = TRUE)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    return(FALSE)
  })
}

# Función de validación de entradas
validate_inputs <- function(input) {
  errors <- character()

  if (input$user == "") errors <- c(errors, "Usuario es requerido")
  if (input$password == "") errors <- c(errors, "Contraseña es requerida")
  if (input$dataset_id == "") errors <- c(errors, "ID del Dataset es requerido")
  if (input$ruta_python == "") errors <- c(errors, "Ruta de Python es requerida")
  if (input$download_path == "") errors <- c(errors, "Ruta de descarga es requerida")

  # Validar formato de fechas
  if (input$start != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$start)) {
      errors <- c(errors, "Formato de fecha de inicio inválido (debe ser YYYY-MM-DD)")
    }
  }

  if (input$end != "") {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", input$end)) {
      errors <- c(errors, "Formato de fecha de fin inválido (debe ser YYYY-MM-DD)")
    }
  }

  # Validar formato bbox si se proporciona
  if (input$bbox != "") {
    bbox_values <- tryCatch({
      as.numeric(unlist(strsplit(input$bbox, ",")))
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(bbox_values) || length(bbox_values) != 4) {
      errors <- c(errors, "Formato de Bbox inválido (debe ser: xmin,ymin,xmax,ymax)")
    }
  }

  return(errors)
}

# Definición de la UI con estilo mejorado
ui <- dashboardPage(
  dashboardHeader(title = "Download VPP"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Búsqueda y Descarga", tabName = "search", icon = icon("search")),
      menuItem("Visualización", tabName = "visualization", icon = icon("image")),
      menuItem("Configuración", tabName = "config", icon = icon("cog")),
      menuItem("Acerca de", tabName = "about", icon = icon("info-circle"))
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
                    column(4, textInput("productGroupId", "ID de Grupo de Producto:"))
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
              ),

              fluidRow(
                box(
                  title = "Log de Operaciones", status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("log_output")
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
              ),
              fluidRow(
                box(
                  title = "Metadatos", status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("image_metadata")
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
              ),
              fluidRow(
                box(
                  title = "Configuración de la Aplicación", status = "primary", solidHeader = TRUE, width = 12,
                  checkboxInput("save_config", "Guardar configuración para futuras sesiones", value = FALSE),
                  actionButton("reset_config", "Restablecer configuración", class = "btn-warning")
                )
              )
      ),

      # Pestaña Acerca de
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Acerca de HRVPP Downloader", status = "primary", solidHeader = TRUE, width = 12,
                  p("Esta aplicación permite buscar y descargar imágenes de alta resolución del conjunto de datos HRVPP."),
                  p("Desarrollada utilizando R Shiny y Python HDA Client."),
                  hr(),
                  h4("Instrucciones de uso:"),
                  tags$ol(
                    tags$li("Configure la ruta de Python y sus credenciales en la pestaña de Búsqueda y Descarga."),
                    tags$li("Establezca los parámetros de búsqueda deseados."),
                    tags$li("Haga clic en 'Buscar' para encontrar imágenes disponibles."),
                    tags$li("Haga clic en 'Descargar' para obtener las imágenes encontradas."),
                    tags$li("Visualice las imágenes descargadas en la pestaña de Visualización.")
                  ),
                  hr(),
                  p("Versión 1.0.0")
                )
              )
      )
    )
  )
)

# Lógica del servidor con manejo de errores mejorado
server <- function(input, output, session) {

  # Valores reactivos para almacenar el estado
  values <- reactiveValues(
    python_configured = FALSE,
    hda_client = NULL,
    search_results = NULL,
    downloaded_files = NULL,
    selected_image = NULL,
    logs = character()
  )

  # Función para agregar entradas al log
  add_log <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- paste0("[", timestamp, "] ", message)
    values$logs <- c(log_entry, values$logs)
    if (length(values$logs) > 100) {
      values$logs <- values$logs[1:100]  # Mantener solo los últimos 100 registros
    }
  }

  # Mostrar logs
  output$log_output <- renderPrint({
    cat(paste(values$logs, collapse = "\n"))
  })

  # Verificar configuración
  observeEvent(input$check_config, {
    add_log("Verificando configuración...")

    # Verificar ruta de Python
    python_ok <- configurar_python(input$ruta_python)
    if (python_ok) {
      values$python_configured <- TRUE
      add_log("Python configurado correctamente")
      showNotification("Python configurado correctamente", type = "message")
    } else {
      add_log("Error al configurar Python. Verifique la ruta.")
      showNotification("Error al configurar Python. Verifique la ruta.", type = "error")
      return()
    }

    # Verificar ruta de descarga
    download_path <- input$download_path
    if (dir.exists(download_path)) {
      add_log("Ruta de descarga verificada")
      showNotification("Ruta de descarga verificada", type = "message")
    } else {
      # Intentar crear el directorio
      dir_created <- dir.create(download_path, recursive = TRUE, showWarnings = FALSE)
      if (dir_created) {
        add_log("Ruta de descarga creada exitosamente")
        showNotification("Ruta de descarga creada exitosamente", type = "message")
      } else {
        add_log("No se pudo crear la ruta de descarga")
        showNotification("No se pudo crear la ruta de descarga", type = "error")
      }
    }

    # Intentar inicializar el cliente HDA
    tryCatch({
      add_log("Importando módulo HDA...")
      hda <- import("hda")
      add_log("Configurando credenciales...")
      conf <- hda$Configuration(user = input$user, password = input$password)
      add_log("Inicializando cliente HDA...")
      values$hda_client <- hda$Client(config = conf)
      add_log("Conexión con HDA establecida exitosamente")
      showNotification("Conexión con HDA establecida", type = "message")
    }, error = function(e) {
      add_log(paste("Error al conectar con HDA:", e$message))
      showNotification(paste("Error al conectar con HDA:", e$message), type = "error")
    })
  })

  # Buscar imágenes
  observeEvent(input$search, {
    add_log("Iniciando búsqueda de imágenes...")

    # Limpiar mensajes de error anteriores
    html("errorMessages", "")

    # Validar entradas
    errors <- validate_inputs(input)
    if (length(errors) > 0) {
      html("errorMessages", paste("<div>", paste(errors, collapse = "</div><div>"), "</div>"))
      add_log("Errores de validación en los parámetros de búsqueda")
      return()
    }

    if (!values$python_configured) {
      add_log("Python no configurado. Por favor verifique la configuración primero.")
      showNotification("Python no configurado. Por favor verifique la configuración primero.", type = "error")
      return()
    }

    if (is.null(values$hda_client)) {
      add_log("Cliente HDA no inicializado. Por favor verifique la configuración primero.")
      showNotification("Cliente HDA no inicializado. Por favor verifique la configuración primero.", type = "error")
      return()
    }

    # Preparar parámetros de consulta
    query <- list(dataset_id = input$dataset_id)

    # Agregar parámetros opcionales si se proporcionan
    if (input$productType != "") query$productType <- input$productType
    if (input$productGroupId != "") query$productGroupId <- input$productGroupId
    if (input$tileId != "") query$tileId <- input$tileId
    if (input$start != "") query$start <- input$start
    if (input$end != "") query$end <- input$end

    # Agregar bbox si se proporciona
    if (input$bbox != "") {
      query$bbox <- as.numeric(unlist(strsplit(input$bbox, ",")))
    }

    add_log(paste("Parámetros de búsqueda:", paste(names(query), query, sep="=", collapse=", ")))

    # Ejecutar búsqueda con indicador de progreso
    withProgress(message = 'Buscando imágenes...', {
      tryCatch({
        add_log("Ejecutando búsqueda...")
        values$search_results <- values$hda_client$search(query)

        if (length(values$search_results) > 0) {
          add_log(paste("Búsqueda exitosa:", length(values$search_results), "imágenes encontradas"))
          showNotification(paste(length(values$search_results), "imágenes encontradas"), type = "message")
        } else {
          add_log("No se encontraron imágenes con los criterios especificados")
          showNotification("No se encontraron imágenes con los criterios especificados", type = "warning")
        }
      }, error = function(e) {
        add_log(paste("Error en la búsqueda:", e$message))
        showNotification(paste("Error en la búsqueda:", e$message), type = "error")
      })
    })
  })

  # Mostrar resultados de búsqueda
  output$result <- renderPrint({
    if (!is.null(values$search_results)) {
      return(values$search_results)
    } else {
      return("No hay resultados para mostrar. Realice una búsqueda primero.")
    }
  })

  # Descargar imágenes
  observeEvent(input$download, {
    add_log("Iniciando proceso de descarga...")

    if (is.null(values$search_results) || length(values$search_results) == 0) {
      add_log("No hay resultados para descargar. Realice una búsqueda primero.")
      showNotification("No hay resultados para descargar. Realice una búsqueda primero.", type = "warning")
      return()
    }

    download_path <- input$download_path
    if (!dir.exists(download_path)) {
      dir.create(download_path, recursive = TRUE, showWarnings = FALSE)
      add_log(paste("Creado directorio de descarga:", download_path))
    }

    # Descargar con seguimiento de progreso
    withProgress(message = 'Descargando imágenes...', {
      # Intentar descargar directamente usando el método de la colección completa
      tryCatch({
        add_log("Descargando imágenes...")
        values$search_results$download(download_path)
        add_log("Descarga completada a través del método de colección")
      }, error = function(e) {
        add_log(paste("Error al descargar usando el método de colección:", e$message))
        add_log("Intentando método alternativo de descarga...")

        # Método alternativo: iterar si la colección es iterable
        tryCatch({
          total_files <- length(values$search_results)
          downloaded_count <- 0

          # Si es una lista de Python (iterable)
          for (item in values$search_results) {
            incProgress(1/total_files)
            downloaded_count <- downloaded_count + 1

            tryCatch({
              add_log(paste("Descargando imagen", downloaded_count, "de", total_files))

              # Intenta diferentes formas de acceder al método de descarga
              if (is.function(item$download)) {
                item$download(download_path)
                add_log(paste("Imagen", downloaded_count, "descargada correctamente"))
              } else {
                add_log(paste("Método de descarga no disponible para el elemento", downloaded_count))
              }
            }, error = function(e) {
              add_log(paste("Error al descargar imagen", downloaded_count, ":", e$message))
            })
          }

          add_log(paste("Procesadas", downloaded_count, "imágenes mediante método alternativo"))
        }, error = function(e) {
          add_log(paste("Error en método alternativo de descarga:", e$message))
          showNotification("Error en la descarga. Vea el log para más detalles.", type = "error")
        })
      })

      # Almacenar archivos descargados independientemente del método usado
      Sys.sleep(1)  # Breve pausa para asegurar que todos los archivos se hayan escrito
      values$downloaded_files <- list.files(download_path, pattern = "\\.tif$|\\.jp2$|\\.jp$|\\.jpg$|\\.png$", full.names = TRUE, recursive = TRUE)

      if (length(values$downloaded_files) > 0) {
        add_log(paste(length(values$downloaded_files), "imágenes descargadas correctamente"))
        showNotification(paste(length(values$downloaded_files), "imágenes encontradas en la carpeta de descarga"), type = "message")
      } else {
        add_log("No se encontraron imágenes en la carpeta de descarga")
        showNotification("No se encontraron imágenes en la carpeta de descarga", type = "warning")
      }
    })
  })

  # Mostrar miniaturas de imágenes descargadas
  output$thumbnails <- renderUI({
    if (!is.null(values$downloaded_files) && length(values$downloaded_files) > 0) {
      thumbnails <- lapply(values$downloaded_files, function(file) {
        # Crear miniatura de manera segura
        thumbnail <- tryCatch({
          img <- image_read(file)
          img_thumbnail <- image_scale(img, "150x")
          thumbnail_path <- file.path(tempdir(), paste0("thumb_", basename(file), ".png"))
          image_write(img_thumbnail, path = thumbnail_path, format = "png")
          thumbnail_path
        }, error = function(e) {
          # Devolver placeholder para miniaturas fallidas
          add_log(paste("Error al crear miniatura para", basename(file), ":", e$message))
          return(NULL)
        })

        filename <- basename(file)

        if (!is.null(thumbnail)) {
          div(class = "thumbnail",
              onclick = paste0("Shiny.setInputValue('selected_file', '", file, "')"),
              tags$img(src = thumbnail, alt = filename),
              div(class = "thumbnail-caption", filename)
          )
        } else {
          div(class = "thumbnail error",
              tags$div(style = "width: 150px; height: 150px; background-color: #f8d7da;
                      display: flex; align-items: center; justify-content: center;",
                       tags$i(class = "fa fa-exclamation-triangle"),
                       "Error en miniatura"),
              div(class = "thumbnail-caption", filename)
          )
        }
      })

      return(div(class = "thumbnail-container", do.call(tagList, thumbnails)))
    } else {
      return(p("No hay imágenes para mostrar. Realice una búsqueda y descarga primero."))
    }
  })

  # Manejar selección de miniatura
  observeEvent(input$selected_file, {
    values$selected_image <- input$selected_file
    add_log(paste("Imagen seleccionada:", basename(input$selected_file)))
  })

  # Mostrar imagen seleccionada
  output$selected_image <- renderPlot({
    if (!is.null(values$selected_image) && file.exists(values$selected_image)) {
      tryCatch({
        img <- image_read(values$selected_image)
        plot(as.raster(img))
      }, error = function(e) {
        # Crear un gráfico de error simple
        add_log(paste("Error al cargar la imagen:", e$message))
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
             main = paste("Error al cargar la imagen:", e$message))
        text(0.5, 0.5, "No se pudo cargar la imagen seleccionada", col = "red")
      })
    } else {
      # Crear un placeholder simple
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
           main = "Seleccione una imagen para visualizar")
      text(0.5, 0.5, "Haga clic en una miniatura para ver la imagen completa")
    }
  })

  # Mostrar metadatos de la imagen
  output$image_metadata <- renderPrint({
    if (!is.null(values$selected_image) && file.exists(values$selected_image)) {
      tryCatch({
        img_info <- image_info(image_read(values$selected_image))
        return(img_info)
      }, error = function(e) {
        return(paste("Error al obtener metadatos:", e$message))
      })
    } else {
      return("Seleccione una imagen para ver sus metadatos.")
    }
  })

  # Mostrar información del sistema
  output$system_info <- renderPrint({
    info <- list(
      "Python configurado" = ifelse(values$python_configured, "Sí", "No"),
      "Ruta de Python" = input$ruta_python,
      "Cliente HDA inicializado" = !is.null(values$hda_client),
      "Usuario" = input$user,
      "Ruta de descarga" = input$download_path,
      "Disponibilidad de ruta de descarga" = dir.exists(input$download_path),
      "Imágenes encontradas" = ifelse(!is.null(values$search_results), length(values$search_results), 0),
      "Imágenes descargadas" = ifelse(!is.null(values$downloaded_files), length(values$downloaded_files), 0),
      "Versiones" = list(
        "R" = R.version$version.string,
        "Shiny" = packageVersion("shiny"),
        "Reticulate" = packageVersion("reticulate"),
        "Magick" = packageVersion("magick"),
        "Sistema" = Sys.info()["sysname"]
      )
    )

    return(info)
  })

  # Restablecer configuración
  observeEvent(input$reset_config, {
    # Limpiar todos los campos de entrada
    updateTextInput(session, "user", value = "")
    updateTextInput(session, "password", value = "")
    updateTextInput(session, "dataset_id", value = "")
    updateTextInput(session, "productType", value = "")
    updateTextInput(session, "productGroupId", value = "")
    updateTextInput(session, "tileId", value = "")
    updateTextInput(session, "start", value = "")
    updateTextInput(session, "end", value = "")
    updateTextInput(session, "bbox", value = "")
    updateTextInput(session, "ruta_python", value = "")
    updateTextInput(session, "download_path", value = "")

    # Restablecer valores reactivos
    values$python_configured <- FALSE
    values$hda_client <- NULL
    values$search_results <- NULL
    values$downloaded_files <- NULL
    values$selected_image <- NULL

    add_log("Configuración restablecida")
    showNotification("Configuración restablecida", type = "message")
  })

  # Al iniciar la app, intentar cargar configuración guardada
  observe({
    if (file.exists(".hrvpp_config.rds")) {
      tryCatch({
        saved_config <- readRDS(".hrvpp_config.rds")
        updateTextInput(session, "user", value = saved_config$user)
        updateTextInput(session, "ruta_python", value = saved_config$ruta_python)
        updateTextInput(session, "download_path", value = saved_config$download_path)
        add_log("Configuración cargada")
      }, error = function(e) {
        add_log("Error al cargar configuración guardada")
      })
    }
  }, priority = 1000)

  # Guardar configuración al salir si está habilitado
  onSessionEnded(function() {
    if (input$save_config) {
      config <- list(
        user = input$user,
        ruta_python = input$ruta_python,
        download_path = input$download_path
      )
      saveRDS(config, ".hrvpp_config.rds")
      add_log("Configuración guardada")
    }
  })

  # Implementación de la función principal Download.HRVPP
  Download.HRVPP <- function(user, password, dataset_id, productType, productGroupId,
                             tileId, start, end, bbox, download_path, ruta_python) {
    # Configurar Python
    python_ok <- configurar_python(ruta_python)
    if (!python_ok) {
      add_log("Error: No se pudo configurar Python")
      return(FALSE)
    }

    tryCatch({
      # Importar hda
      hda <- import("hda")
      # Configurar credenciales de usuario
      conf <- hda$Configuration(user = user, password = password)
      hda_client <- hda$Client(config = conf)

      # Seleccionar parámetros para la descarga
      query <- list(
        dataset_id = dataset_id
      )

      # Agregar parámetros opcionales si no están vacíos
      if (!is.null(productType) && productType != "") query$productType <- productType
      if (!is.null(productGroupId) && productGroupId != "") query$productGroupId <- productGroupId
      if (!is.null(tileId) && tileId != "") query$tileId <- tileId
      if (!is.null(start) && start != "") query$start <- start
      if (!is.null(end) && end != "") query$end <- end
      if (!is.null(bbox) && bbox != "") {
        if (is.character(bbox)) {
          bbox <- as.numeric(unlist(strsplit(bbox, ",")))
        }
        query$bbox <- bbox
      }

      # Enviar solicitud
      matches <- hda_client$search(query)
      add_log(paste("Encontradas", length(matches), "imágenes"))

      # Descargar datos en la ruta especificada
      if (length(matches) > 0) {
        # Intentar descargar toda la colección primero
        tryCatch({
          matches$download(download_path)
          add_log("Descarga completada con método de colección")
        }, error = function(e) {
          add_log(paste("Error en descarga de colección:", e$message))
          add_log("Intentando descarga individual...")

          # Intento alternativo iterando sobre los resultados
          success_count <- 0
          for (item in matches) {
            tryCatch({
              item$download(download_path)
              success_count <- success_count + 1
            }, error = function(e) {
              add_log(paste("Error en descarga individual:", e$message))
            })
          }

          add_log(paste("Completadas", success_count, "descargas individuales de", length(matches)))
        })

        return(TRUE)
      } else {
        add_log("No se encontraron imágenes para descargar")
        return(FALSE)
      }
    }, error = function(e) {
      add_log(paste("Error en Download.HRVPP:", e$message))
      return(FALSE)
    })
  }
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
