# Cargar script global
source("global.R")

server <- function(input, output, session) {
  # ==========================================================================
  # DETECCIÓN AUTOMÁTICA DE TODOS LOS DISCOS LOCALES
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
    withProgress(message = '🔍 Buscando productos HRVPP...', {
      incProgress(0.5, detail = "Procesando consulta...")
      tryCatch({
        query <- list(
          dataset_id = input$dataset_id
        )
        if (input$productType != "") query$productType <- input$productType
        if (input$productGroupId != "") query$productGroupId <- input$productGroupId
        if (input$tileId != "") query$tileId <- input$tileId
        if (input$start != "") query$start <- input$start
        if (input$end != "") query$end <- input$end
        if (input$bbox != "") {
          bbox_vals <- as.numeric(unlist(strsplit(input$bbox, ",")))
          if (length(bbox_vals) == 4) query$bbox <- bbox_vals
        }
        values$search_results <- values$hda_client$search(query)
        incProgress(0.5, detail = "Finalizando búsqueda...")
        if (length(values$search_results) > 0) {
          showNotification(paste("🎉", length(values$search_results), "productos encontrados"), type = "message", duration = 6)
        } else {
          showNotification("⚠️ No se encontraron productos con los criterios especificados", type = "warning", duration = 8)
        }
      }, error = function(e) {
        showNotification(paste("❌ Error en la búsqueda:", e$message), type = "error", duration = 10)
      })
    })
  })

  # ==========================================================================
  # RESULTADOS DE BÚSQUEDA
  # ==========================================================================
  output$result <- renderPrint({
    if (!is.null(values$search_results)) {
      cat("📊 RESULTADOS DE LA BÚSQUEDA\n")
      cat("═══════════════════════════════\n\n")
      cat("🔢 Total de productos encontrados:", length(values$search_results), "\n\n")
      if (length(values$search_results) > 0) {
        cat("📋 Detalles de los primeros 5 resultados:\n")
        cat("─────────────────────────────────────────\n")
        max_show <- min(5, length(values$search_results))
        for (i in 1:max_show) {
          item <- values$search_results[[i]]
          cat(sprintf("🖼️ Producto %d:\n", i))
          cat(sprintf(" 🆔 ID: %s\n", item$id %||% "N/A"))
          cat(sprintf(" 📅 Fecha: %s\n", item$date %||% "N/A"))
          cat(sprintf(" 📏 Tamaño: %s\n\n", item$size %||% "N/A"))
        }
        if (length(values$search_results) > 5) {
          cat(sprintf("... y %d productos más.\n\n", length(values$search_results) - 5))
        }
        cat("💡 Consejo: Use el botón 'Descargar' para obtener todos los productos.\n")
      }
      cat("\n⏰ Búsqueda realizada:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    } else {
      cat("📭 No hay resultados para mostrar.\n")
      cat("═══════════════════════════════\n\n")
      cat("🔍 Para comenzar:\n")
      cat(" 1. Configure Python y las credenciales\n")
      cat(" 2. Complete los parámetros de búsqueda\n")
      cat(" 3. Haga clic en 'Buscar Productos'\n\n")
      cat("💡 Asegúrese de que todos los campos obligatorios estén completos.")
    }
  })

  # ==========================================================================
  # DESCARGA
  # ==========================================================================
  observeEvent(input$download, {
    if (is.null(values$search_results) || length(values$search_results) == 0) {
      showNotification("⚠️ No hay resultados para descargar. Realice una búsqueda primero.", type = "warning", duration = 8)
      return()
    }
    if (is.null(download_path())) {
      showNotification("📁 Seleccione una carpeta de descarga", type = "error", duration = 8)
      return()
    }
    dir.create(download_path(), recursive = TRUE, showWarnings = FALSE)
    withProgress(message = '⬇️ Descargando productos...', {
      total_files <- length(values$search_results)
      failed_downloads <- 0
      for (i in seq_along(values$search_results)) {
        incProgress(1/total_files, detail = sprintf("Descargando %d de %d...", i, total_files))
        tryCatch({
          values$search_results[[i]]$download(download_path())
        }, error = function(e) {
          failed_downloads <<- failed_downloads + 1
        })
      }
      values$downloaded_files <- list.files(download_path(), pattern = "\\.(tif|zip|jpg|png)$", full.names = TRUE, ignore.case = TRUE)
      success_msg <- sprintf("✅ %d productos descargados", length(values$downloaded_files))
      if (failed_downloads > 0) success_msg <- paste(success_msg, sprintf("(%d fallaron)", failed_downloads))
      showNotification(success_msg, type = "message", duration = 8)
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
              h4("📸 Galería de Productos", style = "color: #495057;"),
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

  # ==========================================================================
  # SELECCIÓN DE ARCHIVO
  # ==========================================================================
  observeEvent(input$selected_file, {
    values$selected_image <- input$selected_file
    showNotification(paste("🖼️ Archivo seleccionado:", basename(input$selected_file)), type = "message", duration = 3)
  })

  # ==========================================================================
  # VISUALIZACIÓN DETALLADA
  # ==========================================================================
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
    cat("🖥️ ESTADO DEL SISTEMA - DOWNLOAD HRVPP\n")
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
    cat(sprintf("🔍 Productos encontrados: %d\n", ifelse(!is.null(values$search_results), length(values$search_results), 0)))
    cat(sprintf("⬇️ Archivos descargados: %d\n", ifelse(!is.null(values$downloaded_files), length(values$downloaded_files), 0)))
    cat(sprintf("🖼️ Archivo seleccionado: %s\n", ifelse(!is.null(values$selected_image), basename(values$selected_image), "Ninguno")))
    cat("\n🔧 INFORMACIÓN DEL SISTEMA:\n")
    cat("─────────────────────────────\n")
    cat(sprintf("📅 Fecha actual: %s\n", format(Sys.Date(), "%Y-%m-%d")))
    cat(sprintf("⏰ Hora actual: %s\n", format(Sys.time(), "%H:%M:%S")))
    cat("\n📋 RESUMEN DEL ESTADO:\n")
    cat("─────────────────────────────\n")
    system_status <- "✅ OPERATIVO"
    if (!values$python_configured) system_status <- "⚠️ CONFIGURACIÓN PENDIENTE"
    if (is.null(values$hda_client)) system_status <- "❌ DESCONECTADO"
    cat(sprintf("🚥 Estado general: %s\n", system_status))
    cat(sprintf("⏱️ Última actualización: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    cat("\n" , rep("═", 40), "\n")
    cat("💡 Sugerencia: Mantenga todas las configuraciones actualizadas para un funcionamiento óptimo.\n")
  })
}
