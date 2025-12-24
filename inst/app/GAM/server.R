# Cargar script global
source("global.R")

# server.R --------------------------------------------------------------------
server <- function(input, output, session) {

  data <- reactiveVal(NULL)
  gam_result <- reactiveVal(NULL)

  # Cargar CSV
  observeEvent(input$csv_file, {
    req(input$csv_file)
    withProgress(message = '📂 Leyendo archivo...', {
      incProgress(0.3, detail = "Detectando separador...")
      tryCatch({
        sep <- switch(input$sep_mode,
                      "auto" = ",",
                      input$sep_mode)
        if (input$sep_mode == "auto") {
          df <- read_delim(input$csv_file$datapath, delim = guess_delim(input$csv_file$datapath), progress = FALSE)
        } else {
          df <- read_delim(input$csv_file$datapath, delim = sep, progress = FALSE)
        }
        data(df)
        updateSelectInput(session, "year_col", choices = names(df))
        updateSelectInput(session, "doy_col", choices = names(df))
        updateSelectInput(session, "missing_col", choices = names(df))
        showNotification("✅ Archivo cargado correctamente", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error al leer:", e$message), type = "error", duration = 10)
      })
    })
  })

  output$data_preview <- DT::renderDT({
    req(data())
    DT::datatable(head(data(), 100), options = list(scrollX = TRUE, pageLength = 10))
  })

  # Ejecutar GAM
  observeEvent(input$run_gam, {
    req(data())
    req(input$year_col, input$doy_col, input$missing_col)
    if (!all(c(input$year_col, input$doy_col, input$missing_col) %in% names(data()))) {
      showNotification("❌ Columna(s) no encontrada(s)", type = "error")
      return()
    }
    withProgress(message = '📊 Ejecutando modelo GAM...', {
      incProgress(0.3, detail = "Preparando datos...")
      result <- tryCatch({
        GAM.missing(
          data = data(),
          year_col = input$year_col,
          doy_col = input$doy_col,
          missing_col = input$missing_col
        )
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error")
        NULL
      })
      incProgress(0.7, detail = "Finalizando...")
      if (!is.null(result)) {
        gam_result(result)
        showNotification("✅ Modelo ejecutado correctamente", type = "message", duration = 6)
      }
    })
  })

  # Tabla de resultados
  output$gam_table <- DT::renderDT({
    req(gam_result())
    DT::datatable(
      gam_result()$proporciones,
      options = list(scrollX = TRUE, pageLength = 20),
      rownames = FALSE
    ) %>%
      DT::formatRound(c("Proporcion_Missing", "Predicted"), digits = 4)
  })

  # Gráfico
  output$gam_plot <- renderPlot({
    req(gam_result())
    df <- gam_result()$proporciones
    ggplot(df, aes(x = !!sym(input$doy_col))) +
      geom_point(aes(y = Proporcion_Missing, color = "Observado"), size = 2, alpha = 0.7) +
      geom_line(aes(y = Predicted, color = "Predicción GAM"), size = 1.2) +
      facet_wrap(as.formula(paste("~", input$year_col)), scales = "free_y") +
      scale_color_manual(values = c("Observado" = "#e74c3c", "Predicción GAM" = "#3498db")) +
      labs(
        title = "Proporción de Datos Faltantes por Día del Año",
        subtitle = "Suavizado con Modelo GAM (binomial)",
        x = "Día del Año (DOY)",
        y = "Proporción de Missings",
        color = "Leyenda"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#2c3e50"),
        legend.position = "top",
        strip.background = element_rect(fill = "#667eea", color = "white"),
        strip.text = element_text(color = "white", face = "bold")
      )
  })

  # Evaluación del ajuste
  output$model_summary <- renderPrint({
    req(gam_result())
    model <- gam_result()$model
    s <- summary(model)
    cat("📐 EVALUACIÓN DEL AJUSTE DEL MODELO\n")
    cat(rep("=", 50), "\n\n")
    cat(sprintf("📈 Deviance explained: %.1f%%\n", 100 * s$dev.expl))
    cat(" → Cuanto más alto, mejor explica el modelo los datos\n\n")
    cat(sprintf("📏 EDF (suavizado DOY): %.2f\n", s$edf[1]))
    cat(" → EDF ≈ 1 = relación lineal | EDF > 1 = patrón curvo/estacional\n\n")
    cat(sprintf("🔍 p-value suavizado DOY: %.2e\n", s$s.pv[1]))
    cat(" → p < 0.05 = patrón estacional significativo\n\n")
    cat(sprintf("⚖️ AIC: %.2f\n", AIC(model)))
    cat(" → Valor más bajo = mejor modelo (para comparar)\n")
  })
}
