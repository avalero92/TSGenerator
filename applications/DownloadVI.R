library(shiny)
library(reticulate)
library(magick)  # For image handling
library(shinydashboard)  # For improved UI
library(shinyjs)  # For enhanced JavaScript functionality
library(shinycssloaders)  # For loading spinners

# Python configuration function
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

# Input validation function
validate_inputs <- function(input) {
  errors <- character()

  if (input$user == "") errors <- c(errors, "Usuario es requerido")
  if (input$password == "") errors <- c(errors, "Contraseña es requerida")
  if (input$dataset_id == "") errors <- c(errors, "ID del Dataset es requerido")
  if (input$download_path == "") errors <- c(errors, "Ruta de descarga es requerida")
  if (input$ruta_python == "") errors <- c(errors, "Ruta de Python es requerida")

  # Validate date format
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

  # Validate bbox format if provided
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

# UI definition with improved styling
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
      # Search and Download tab
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

      # Visualization tab
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

      # Configuration tab
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

# Server logic with improved error handling
server <- function(input, output, session) {

  # Reactive values to store state
  values <- reactiveValues(
    python_configured = FALSE,
    hda_client = NULL,
    search_results = NULL,
    downloaded_files = NULL,
    selected_image = NULL
  )

  # Check configuration
  observeEvent(input$check_config, {
    # Check Python path
    python_ok <- configurar_python(input$ruta_python)
    if (python_ok) {
      values$python_configured <- TRUE
      showNotification("Python configurado correctamente", type = "message")
    } else {
      showNotification("Error al configurar Python. Verifique la ruta.", type = "error")
      return()
    }

    # Check download path
    download_path <- input$download_path
    if (dir.exists(download_path)) {
      showNotification("Ruta de descarga verificada", type = "message")
    } else {
      # Try to create the directory
      dir_created <- dir.create(download_path, recursive = TRUE, showWarnings = FALSE)
      if (dir_created) {
        showNotification("Ruta de descarga creada exitosamente", type = "message")
      } else {
        showNotification("No se pudo crear la ruta de descarga", type = "error")
      }
    }

    # Try to initialize HDA client
    tryCatch({
      hda <- import("hda")
      conf <- hda$Configuration(user = input$user, password = input$password)
      values$hda_client <- hda$Client(config = conf)
      showNotification("Conexión con HDA establecida", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al conectar con HDA:", e$message), type = "error")
    })
  })

  # Search for images
  observeEvent(input$search, {
    # Clear previous error messages
    html("errorMessages", "")

    # Validate inputs
    errors <- validate_inputs(input)
    if (length(errors) > 0) {
      html("errorMessages", paste("<div>", paste(errors, collapse = "</div><div>"), "</div>"))
      return()
    }

    if (!values$python_configured) {
      showNotification("Python no configurado. Por favor verifique la configuración primero.", type = "error")
      return()
    }

    if (is.null(values$hda_client)) {
      showNotification("Cliente HDA no inicializado. Por favor verifique la configuración primero.", type = "error")
      return()
    }

    # Prepare query parameters
    query <- list(dataset_id = input$dataset_id)

    # Add optional parameters if provided
    if (input$productType != "") query$productType <- input$productType
    if (input$platformSerialIdentifier != "") query$platformSerialIdentifier <- input$platformSerialIdentifier
    if (input$tileId != "") query$tileId <- input$tileId
    if (input$start != "") query$start <- input$start
    if (input$end != "") query$end <- input$end

    # Add bbox if provided
    if (input$bbox != "") {
      query$bbox <- as.numeric(unlist(strsplit(input$bbox, ",")))
    }

    # Execute search with progress indicator
    withProgress(message = 'Buscando imágenes...', {
      tryCatch({
        values$search_results <- values$hda_client$search(query)
        if (length(values$search_results) > 0) {
          showNotification(paste(length(values$search_results), "imágenes encontradas"), type = "message")
        } else {
          showNotification("No se encontraron imágenes con los criterios especificados", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error en la búsqueda:", e$message), type = "error")
      })
    })
  })

  # Display search results
  output$result <- renderPrint({
    if (!is.null(values$search_results)) {
      return(values$search_results)
    } else {
      return("No hay resultados para mostrar. Realice una búsqueda primero.")
    }
  })

  # Download images
  observeEvent(input$download, {
    if (is.null(values$search_results) || length(values$search_results) == 0) {
      showNotification("No hay resultados para descargar. Realice una búsqueda primero.", type = "warning")
      return()
    }

    download_path <- input$download_path
    if (!dir.exists(download_path)) {
      dir.create(download_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Download with progress tracking
    withProgress(message = 'Descargando imágenes...', {
      total_files <- length(values$search_results)
      downloaded_files <- character()

      for (i in seq_along(values$search_results)) {
        incProgress(1/total_files, detail = paste("Descargando", i, "de", total_files))

        tryCatch({
          # Assuming the download method returns the path of downloaded file
          file_path <- values$search_results[[i]]$download(download_path)
          downloaded_files <- c(downloaded_files, file_path)
        }, error = function(e) {
          showNotification(paste("Error al descargar imagen", i, ":", e$message), type = "error")
        })
      }

      # Store downloaded files
      values$downloaded_files <- list.files(download_path, pattern = "\\.tif$", full.names = TRUE)

      if (length(values$downloaded_files) > 0) {
        showNotification(paste(length(values$downloaded_files), "imágenes descargadas correctamente"), type = "message")
      } else {
        showNotification("No se pudieron descargar las imágenes", type = "error")
      }
    })
  })

  # Display thumbnails of downloaded images
  output$thumbnails <- renderUI({
    if (!is.null(values$downloaded_files) && length(values$downloaded_files) > 0) {
      thumbnails <- lapply(values$downloaded_files, function(file) {
        # Create thumbnail safely
        thumbnail <- tryCatch({
          img <- image_read(file)
          img_thumbnail <- image_scale(img, "150x")
          thumbnail_path <- file.path(tempdir(), paste0("thumb_", basename(file), ".png"))
          image_write(img_thumbnail, path = thumbnail_path, format = "png")
          thumbnail_path
        }, error = function(e) {
          # Return placeholder for failed thumbnails
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

  # Handle thumbnail selection
  observeEvent(input$selected_file, {
    values$selected_image <- input$selected_file
  })

  # Display selected image
  output$selected_image <- renderPlot({
    if (!is.null(values$selected_image) && file.exists(values$selected_image)) {
      tryCatch({
        img <- image_read(values$selected_image)
        plot(as.raster(img))
      }, error = function(e) {
        # Create a simple error plot
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
             main = paste("Error al cargar la imagen:", e$message))
        text(0.5, 0.5, "No se pudo cargar la imagen seleccionada", col = "red")
      })
    } else {
      # Create a simple placeholder
      plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
           main = "Seleccione una imagen para visualizar")
      text(0.5, 0.5, "Haga clic en una miniatura para ver la imagen completa")
    }
  })

  # Display system information
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
        "Magick" = packageVersion("magick")
      )
    )

    return(info)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


