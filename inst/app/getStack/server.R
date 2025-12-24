# Cargar script global
source("global.R")
# server.R --------------------------------------------------------------------
server <- function(input, output, session) {

  # Selector de carpetas con todos los discos locales (definidos en global.R)
  shinyDirChoose(input, "iv_folder", roots = volumes, allowDirCreate = TRUE)
  shinyDirChoose(input, "qflag_folder", roots = volumes, allowDirCreate = TRUE)
  shinyDirChoose(input, "output_folder", roots = volumes, allowDirCreate = TRUE)

  iv_path <- reactive({
    req(input$iv_folder)
    parseDirPath(roots = volumes, input$iv_folder)
  })

  qflag_path <- reactive({
    req(input$qflag_folder)
    parseDirPath(roots = volumes, input$qflag_folder)
  })

  output_path <- reactive({
    req(input$output_folder)
    parseDirPath(roots = volumes, input$output_folder)
  })

  output$iv_path_display <- renderPrint({
    cat(ifelse(is.null(input$iv_folder), "Ninguna carpeta seleccionada", iv_path()))
  })

  output$qflag_path_display <- renderPrint({
    cat(ifelse(is.null(input$qflag_folder), "Ninguna carpeta seleccionada", qflag_path()))
  })

  output$output_path_display <- renderPrint({
    cat(ifelse(is.null(input$output_folder), "Ninguna carpeta seleccionada", output_path()))
  })

  # Valores reactivos para almacenar resultados
  values <- reactiveValues(
    processed = 0,
    matched = 0,
    missing_qflag = 0,
    dimension_mismatch = 0,
    log_messages = character()
  )

  # Función get.Stack() integrada directamente
  get.Stack <- function(IV_path, QFLAG, output_path, pattern = "\\.tif$", use_parallel = TRUE) {
    archivos <- list.files(IV_path, pattern = pattern, full.names = TRUE)
    if (length(archivos) == 0) {
      values$log_messages <<- c(values$log_messages, "⚠️ No se encontraron archivos .tif en la carpeta IV")
      return()
    }
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    values$processed <<- length(archivos)
    values$matched <<- 0
    values$missing_qflag <<- 0
    values$dimension_mismatch <<- 0

    process_file <- function(archivo) {
      fecha <- gsub(".*_(\\d{8})_.*", "\\1", basename(archivo))
      archivo_qflag <- file.path(QFLAG, paste0(fecha, ".tif")) # Asumiendo .tif también
      if (file.exists(archivo_qflag)) {
        raster_iv <- brick(archivo)
        qflag <- brick(archivo_qflag)
        qflag_resampled <- raster::resample(qflag, raster_iv, method = "bilinear")
        if (nrow(raster_iv) == nrow(qflag_resampled) && ncol(raster_iv) == ncol(qflag_resampled)) {
          stack_final <- addLayer(raster_iv, qflag_resampled)
          output_file <- file.path(output_path, paste0("stack_", fecha, ".tif"))
          writeRaster(stack_final, output_file, format = "GTiff", overwrite = TRUE)
          values$matched <<- values$matched + 1
          paste("✅ Stack creado para fecha", fecha)
        } else {
          values$dimension_mismatch <<- values$dimension_mismatch + 1
          paste("❌ Dimensiones no coinciden para fecha", fecha)
        }
      } else {
        values$missing_qflag <<- values$missing_qflag + 1
        paste("❌ QFLAG no encontrado para fecha", fecha)
      }
    }

    if (use_parallel && length(archivos) > 1) {
      cores <- detectCores() - 1
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      results <- foreach(archivo = archivos, .combine = c) %dopar% {
        process_file(archivo)
      }
      stopCluster(cl)
    } else {
      results <- sapply(archivos, process_file)
    }

    values$log_messages <<- c(values$log_messages, results)
  }

  # Ejecutar el proceso
  observeEvent(input$run_stack, {
    html("errorMessages", "")
    errors <- character()
    if (is.null(input$iv_folder)) errors <- c(errors, "📥 Seleccione carpeta con archivos IV")
    if (is.null(input$qflag_folder)) errors <- c(errors, "📂 Seleccione carpeta con QFLAG")
    if (is.null(input$output_folder)) errors <- c(errors, "📤 Seleccione carpeta de salida")
    if (length(errors) > 0) {
      error_html <- paste(
        "<div class='alert alert-danger' role='alert'>",
        "<h5>⚠️ Errores:</h5><ul>",
        paste("<li>", errors, "</li>", collapse = ""),
        "</ul></div>"
      )
      html("errorMessages", error_html)
      return()
    }

    withProgress(message = '🛠️ Creando stacks IV + QFLAG...', value = 0, {
      incProgress(0.1, detail = "Inicializando...")
      get.Stack(
        IV_path = iv_path(),
        QFLAG = qflag_path(),
        output_path = output_path(),
        pattern = input$pattern,
        use_parallel = input$use_parallel
      )
      incProgress(0.9, detail = "Finalizando...")
      showNotification(
        paste("✅ Procesamiento completado:", values$matched, "stacks creados"),
        type = "message",
        duration = 8
      )
    })
  })

  # Resumen de resultados
  output$processing_summary <- renderPrint({
    cat("📊 RESUMEN DEL PROCESAMIENTO - GET STACK\n")
    cat(rep("=", 55), "\n\n")
    cat("📂 Carpeta IV: ", ifelse(is.null(input$iv_folder), "No seleccionada", iv_path()), "\n")
    cat("📂 Carpeta QFLAG: ", ifelse(is.null(input$qflag_folder), "No seleccionada", qflag_path()), "\n")
    cat("📤 Carpeta salida: ", ifelse(is.null(input$output_folder), "No seleccionada", output_path()), "\n\n")
    if (values$processed > 0) {
      cat("🔢 Archivos IV procesados: ", values$processed, "\n")
      cat("✅ Stacks creados correctamente: ", values$matched, "\n")
      cat("❌ QFLAG no encontrado: ", values$missing_qflag, "\n")
      cat("❌ Dimensiones no coincidentes: ", values$dimension_mismatch, "\n\n")
      if (values$matched > 0) {
        cat("✨ Los stacks se guardaron en:\n ", output_path(), "\n")
      }
      if (length(values$log_messages) > 0) {
        cat("\n📋 Detalle por archivo:\n")
        cat(paste(values$log_messages, collapse = "\n"), "\n")
      }
    } else {
      cat("📭 Aún no se ha ejecutado ningún procesamiento.\n\n")
      cat("👉 Vaya a la pestaña 'Crear Stack' y haga clic en 'Crear Stacks IV + QFLAG'\n")
    }
    cat("\n⏰ Última ejecución:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
}
