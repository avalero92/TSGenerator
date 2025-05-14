library(shiny)
library(reticulate)
library(magick)  # Para manejar imágenes

# Configuración de Python
configurar_python <- function(ruta_python) {
  if (file.exists(ruta_python)) {
    use_python(ruta_python, required = TRUE)
    message("Python configurado correctamente en: ", ruta_python)
  } else {
    stop("La ruta especificada no existe: ", ruta_python)
  }
}

# Definición de la interfaz de usuario
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
      }
      .sidebar {
        background-color: #343a40;
        color: white;
      }
      .sidebar .form-group {
        margin-bottom: 15px;
      }
      .btn-primary {
        background-color: #007bff;
      }
      h2 {
        color: #007bff;
      }
      .image-container {
        text-align: center;
        margin-top: 20px;
      }
      .thumbnail {
        display: inline-block;
        margin: 10px;
      }
      .thumbnail img {
        width: 100px; /* Tamaño de la miniatura */
        height: auto;
      }
    "))
  ),
  
  titlePanel("DownloadVI"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      textInput("user", "Usuario:"),
      passwordInput("password", "Contraseña:"),
      textInput("dataset_id", "ID del Dataset:"),
      textInput("productType", "Tipo de Producto:"),
      textInput("platformSerialIdentifier", "Identificador de Plataforma:"),
      textInput("tileId", "ID del Tile:"),
      textInput("start", "Fecha de Inicio (YYYY-MM-DD):"),
      textInput("end", "Fecha de Fin (YYYY-MM-DD):"),
      textInput("bbox", "Bbox (xmin, ymin, xmax, ymax):"),
      textInput("download_path", "Ruta de Descarga:"),
      textInput("ruta_python", "Ruta de Python:"),
      actionButton("download", "Descargar", class = "btn-primary")
    ),
    
    mainPanel(
      verbatimTextOutput("result"),
      div(class = "image-container", uiOutput("thumbnails"))
    )
  )
)

# Definición del servidor
server <- function(input, output) {
  
  observeEvent(input$download, {
    ruta_python <- input$ruta_python
    configurar_python(ruta_python)
    
    hda <- import("hda")
    conf <- hda$Configuration(user = input$user, password = input$password)
    hda_client <- hda$Client(config = conf)
    
    query <- list(
      dataset_id = input$dataset_id,
      productType = input$productType,
      platformSerialIdentifier = input$platformSerialIdentifier,
      tileId = input$tileId,
      start = input$start,
      end = input$end,
      bbox = as.numeric(unlist(strsplit(input$bbox, ",")))
    )
    
    matches <- hda_client$search(query)
    output$result <- renderPrint({
      print(matches)
    })
    
    # Iniciar la descarga con una barra de progreso
    withProgress(message = 'Descargando...', {
      for (i in seq_along(matches)) {
        matches$download(input$download_path)
        incProgress(1 / length(matches), detail = paste("Descargando imagen", i, "de", length(matches)))
      }
    })
    
    showNotification("Descarga completada!", type = "message")
    
    # Mostrar miniaturas de las imágenes TIFF descargadas
    tiff_files <- list.files(input$download_path, pattern = "\\.tif$", full.names = TRUE)
    output$thumbnails <- renderUI({
      thumbnails <- lapply(tiff_files, function(file) {
        # Crear miniatura
        img <- image_read(file)
        img_thumbnail <- image_scale(img, "100x")  # Redimensionar a 100px de ancho
        thumbnail_path <- tempfile(fileext = ".png")  # Guardar la miniatura temporalmente
        image_write(img_thumbnail, path = thumbnail_path, format = "png")
        
        # Generar HTML para la miniatura
        tags$div(class = "thumbnail", 
                 tags$img(src = thumbnail_path, alt = basename(file)))
      })
      do.call(tagList, thumbnails)
    })
  })
}

# Ejecución de la aplicación Shiny
shinyApp(ui = ui, server = server)


