function(input, output, session) {
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
        incProgress(1 / total_files, detail = paste("Descargando", i, "de", total_files))
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