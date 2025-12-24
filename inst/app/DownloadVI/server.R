# Cargar script global
source("global.R")

server <- function(input, output, session) {
  # ==========================================================================
  # VALORES REACTIVOS
  # ==========================================================================
  # Almacena el estado de la aplicación y los datos compartidos entre funciones
  values <- reactiveValues(
    python_configured = FALSE, # Estado de configuración de Python
    hda_client = NULL, # Cliente HDA inicializado
    search_results = NULL, # Resultados de la última búsqueda
    downloaded_files = NULL, # Lista de archivos descargados
    selected_image = NULL # Imagen seleccionada para visualización
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
          cat(sprintf("🖼️ Imagen %d:\n", i))
          cat(sprintf(" 📅 Fecha: %s\n",
                      ifelse(is.null(values$search_results[[i]]$date), "N/A", values$search_results[[i]]$date)))
          cat(sprintf(" 🆔 ID: %s\n",
                      ifelse(is.null(values$search_results[[i]]$id), "N/A", values$search_results[[i]]$id)))
          cat(sprintf(" 📏 Tamaño: %s\n",
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
      cat(" 1. Configure Python y las credenciales\n")
      cat(" 2. Complete los parámetros de búsqueda\n")
      cat(" 3. Haga clic en 'Buscar Imágenes'\n\n")
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
          img_thumbnail <- image_scale(img, "200x150!") # Tamaño fijo
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
    cat("🖥️ ESTADO DEL SISTEMA - DOWNLOADVI\n")
    cat("═══════════════════════════════════════════\n\n")
    # Configuración básica
    cat("⚙️ CONFIGURACIÓN BÁSICA:\n")
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
    cat(sprintf("⬇️ Imágenes descargadas: %d\n",
                ifelse(!is.null(values$downloaded_files), length(values$downloaded_files), 0)))
    cat(sprintf("🖼️ Imagen seleccionada: %s\n",
                ifelse(!is.null(values$selected_image), basename(values$selected_image), "Ninguna")))
    # Información del sistema
    cat("\n🔧 INFORMACIÓN DEL SISTEMA:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("📅 Fecha actual: %s\n", format(Sys.Date(), "%Y-%m-%d")))
    cat(sprintf("⏰ Hora actual: %s\n", format(Sys.time(), "%H:%M:%S")))
    cat(sprintf("💾 Directorio temporal: %s\n", tempdir()))
    cat(sprintf("👥 Usuario del sistema: %s\n", Sys.info()["user"]))
    cat(sprintf("🖥️ Sistema operativo: %s\n", Sys.info()["sysname"]))
    # Versiones de paquetes
    cat("\n📦 VERSIONES DE PAQUETES:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("🔵 R: %s\n", R.version$version.string))
    cat(sprintf("✨ Shiny: %s\n", as.character(packageVersion("shiny"))))
    cat(sprintf("🐍 Reticulate: %s\n", as.character(packageVersion("reticulate"))))
    cat(sprintf("🖼️ Magick: %s\n", as.character(packageVersion("magick"))))
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
    if (!values$python_configured) system_status <- "⚠️ CONFIGURACIÓN PENDIENTE"
    if (is.null(values$hda_client)) system_status <- "❌ DESCONECTADO"
    cat(sprintf("🚥 Estado general: %s\n", system_status))
    cat(sprintf("⏱️ Última actualización: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    cat("\n" , rep("═", 40), "\n")
    cat("💡 Sugerencia: Mantenga todas las configuraciones actualizadas\n")
    cat(" para un funcionamiento óptimo de la aplicación.\n")
  })
}

shinyApp(ui = ui, server = server)
