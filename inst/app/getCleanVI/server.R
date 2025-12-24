# Cargar script global
source("global.R")

# server.R --------------------------------------------------------------------
server <- function(input, output, session) {

  # Selección de carpetas (solo home por simplicidad, pero puedes ampliar como en la anterior app)
  shinyDirChoose(input, "stack_folder", roots = c(home = fs::path_home()))
  shinyDirChoose(input, "output_folder", roots = c(home = fs::path_home()))

  stack_folder_path <- reactive({
    if (is.null(input$stack_folder)) return(NULL)
    parseDirPath(roots = c(home = fs::path_home()), input$stack_folder)
  })

  output_folder_path <- reactive({
    if (is.null(input$output_folder)) return(NULL)
    parseDirPath(roots = c(home = fs::path_home()), input$output_folder)
  })

  output$stack_path_display <- renderPrint({
    cat(ifelse(is.null(stack_folder_path()), "Ninguna carpeta seleccionada", stack_folder_path()))
  })

  output$output_path_display <- renderPrint({
    cat(ifelse(is.null(output_folder_path()), "Ninguna carpeta seleccionada", output_folder_path()))
  })

  # Valores reactivos
  values <- reactiveValues(
    last_result = NULL,
    processing_log = ""
  )

  # Ejecutar la máscara
  observeEvent(input$run_mask, {
    html("errorMessages", "")
    errors <- character()

    if (is.null(stack_folder_path()) || stack_folder_path() == "") {
      errors <- c(errors, "📥 Debe seleccionar una carpeta de entrada")
    }
    if (is.null(output_folder_path()) || output_folder_path() == "") {
      errors <- c(errors, "📤 Debe seleccionar una carpeta de salida")
    }

    valid_vals <- tryCatch({
      as.numeric(unlist(strsplit(trimws(input$valid_values), ",")))
    }, error = function(e) NULL)

    if (is.null(valid_vals) || length(valid_vals) == 0) {
      errors <- c(errors, "🔢 Valores válidos deben ser números separados por comas")
    }

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

    withProgress(message = '🛠️ Aplicando máscara QFLAG2...', value = 0, {
      incProgress(0.1, detail = "Preparando parámetros...")

      result <- tryCatch({
        QFLAG2.Mask(
          stack_folder = stack_folder_path(),
          output_folder = output_folder_path(),
          valid_values = valid_vals,
          pattern = input$pattern,
          qflag_band = input$qflag_band,
          target_band = input$target_band,
          use_terra = as.logical(input$use_terra),
          verbose = input$verbose
        )
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
        NULL
      })

      incProgress(0.9, detail = "Finalizando...")
      values$last_result <- result

      if (!is.null(result)) {
        showNotification(
          paste("✅ Procesamiento completado:", result$processed, "archivos procesados"),
          type = "message",
          duration = 8
        )
      }
    })
  })

  # Resumen de resultados
  output$processing_summary <- renderPrint({
    if (is.null(values$last_result)) {
      cat("📭 Aún no se ha ejecutado ningún procesamiento.\n\n")
      cat("👉 Vaya a la pestaña 'Aplicar Máscara' y haga clic en 'Aplicar Máscara QFLAG2'\n")
    } else {
      res <- values$last_result
      cat("📊 RESUMEN DEL ÚLTIMO PROCESAMIENTO\n")
      cat(rep("=", 50), "\n\n")
      cat("📂 Carpeta entrada: ", res$stack_folder %||% stack_folder_path(), "\n")
      cat("📁 Carpeta salida: ", res$output_folder, "\n\n")
      cat("🔢 Archivos encontrados: ", res$total_files, "\n")
      cat("✅ Procesados correctamente:", res$processed, "\n")
      cat("⏭️ Saltados: ", res$skipped, "\n")
      cat("❌ Fallidos: ", res$failed, "\n\n")
      if (res$processed > 0) {
        cat("✨ Los archivos enmascarados se guardaron en:\n")
        cat(" ", res$output_folder, "\n")
      }
      if (res$failed > 0) {
        cat("\n⚠️ Revise las advertencias en la consola para detalles de errores.\n")
      }
      cat("\n⏰ Procesamiento finalizado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    }
  })
}
