# Cargar script global
source("global.R")

# server.R --------------------------------------------------------------------
server <- function(input, output, session) {

  # Selector de carpeta con todos los discos locales (definidos en global.R)
  shinyDirChoose(input, "input_folder", roots = volumes, allowDirCreate = FALSE)

  input_path <- reactive({
    req(input$input_folder)
    parseDirPath(roots = volumes, input$input_folder)
  })

  output$input_path_display <- renderPrint({
    if (is.null(input$input_folder)) {
      cat("Ninguna carpeta seleccionada")
    } else {
      cat(input_path())
    }
  })

  # Valores reactivos para resultados
  values <- reactiveValues(
    renamed_count = 0,
    log_messages = character(),
    last_folder = NULL
  )

  # Función renames.image.IV (integrada y mejorada)
  renames.image.IV <- function(input_folder) {
    if (is.null(input_folder) || !dir.exists(input_folder)) {
      stop("La carpeta especificada no existe o no está seleccionada.")
    }
    archivos <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
    if (length(archivos) == 0) {
      stop("No se encontraron archivos .tif en la carpeta.")
    }
    values$renamed_count <<- 0
    values$log_messages <<- character()
    for (archivo in archivos) {
      basename_file <- basename(archivo)
      # Extraer fecha YYYYMMDD del nombre
      match <- regexpr("\\d{8}", basename_file)
      if (match == -1) {
        values$log_messages <<- c(values$log_messages, paste("⚠️ No se encontró fecha en:", basename_file))
        next
      }
      fecha <- regmatches(basename_file, match)
      new_name <- file.path(input_folder, paste0(fecha, ".tif"))
      if (file.rename(archivo, new_name)) {
        values$renamed_count <<- values$renamed_count + 1
        values$log_messages <<- c(values$log_messages, paste("✅ Renombrado:", basename_file, "→", paste0(fecha, ".tif")))
      } else {
        values$log_messages <<- c(values$log_messages, paste("❌ Falló renombrado:", basename_file))
      }
    }
    if (values$renamed_count == 0) {
      stop("No se pudo renombrar ningún archivo (posible conflicto de nombres o permisos).")
    }
  }

  # Ejecutar renombrado
  observeEvent(input$run_rename, {
    html("errorMessages", "")
    if (is.null(input$input_folder)) {
      error_html <- "<div class='alert alert-danger'><h5>⚠️ Error:</h5><ul><li>📁 Debe seleccionar una carpeta</li></ul></div>"
      html("errorMessages", error_html)
      return()
    }
    withProgress(message = '🖼️ Renombrando imágenes...', value = 0, {
      incProgress(0.2, detail = "Analizando archivos...")
      result <- tryCatch({
        renames.image.IV(input_path())
        values$last_folder <<- input_path()
        "Proceso completado con éxito."
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
        paste("Error:", e$message)
      })
      incProgress(0.8, detail = "Finalizando...")
    })
  })

  # Resumen de resultados
  output$rename_summary <- renderPrint({
    cat("📊 RESUMEN DEL RENOMBRADO - TIMES SERIES VI\n")
    cat(rep("=", 55), "\n\n")
    if (is.null(values$last_folder)) {
      cat("📭 Aún no se ha ejecutado ningún renombrado.\n\n")
      cat("👉 Vaya a la pestaña 'Renombrar Imágenes' y haga clic en 'Renombrar Imágenes'\n")
    } else {
      cat("📁 Carpeta procesada:\n ", values$last_folder, "\n\n")
      cat("✅ Archivos renombrados correctamente: ", values$renamed_count, "\n")
      cat("📄 Total de archivos .tif encontrados: ", length(values$log_messages) + values$renamed_count - length(grep("⚠️|❌", values$log_messages)), "\n\n")
      if (length(values$log_messages) > 0) {
        cat("📋 Detalle por archivo:\n")
        cat("────────────────────────────────────\n")
        cat(paste(values$log_messages, collapse = "\n"), "\n")
      }
      cat("\n⏰ Procesamiento finalizado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    }
  })
}
