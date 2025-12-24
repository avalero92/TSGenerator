library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(dplyr)
library(mgcv)
library(readr)

# ============================================================================
# UI
# ============================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-chart-line", style = "margin-right: 8px;"),
      "GAM Missing",
      style = "font-weight: bold; font-size: 18px;"
    ),
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("⚙️ Cargar Datos",
               tabName = "load",
               icon = icon("upload"),
               badgeLabel = "Primero",
               badgeColor = "orange"),
      menuItem("📊 Análisis GAM",
               tabName = "gam",
               icon = icon("chart-line"),
               badgeLabel = "Principal",
               badgeColor = "blue"),
      menuItem("📈 Resultados",
               tabName = "results",
               icon = icon("table"),
               badgeLabel = "Tabla",
               badgeColor = "green"),
      menuItem("📉 Gráfico",
               tabName = "plot",
               icon = icon("image"),
               badgeLabel = "Visual",
               badgeColor = "purple"),
      menuItem("📐 Ajuste del Modelo",
               tabName = "fit",
               icon = icon("ruler"),
               badgeLabel = "Evaluación",
               badgeColor = "red"),
      br(),
      div(style = "padding: 15px;",
          h5("💡 Consejos rápidos", style = "color: #fff; font-weight: bold;"),
          p("• CSV con cualquier separador (auto-detectado)", style = "color: #bbb; font-size: 12px;"),
          p("• DOY = día del año (1-366)", style = "color: #bbb; font-size: 12px;"),
          p("• Ahora con evaluación del ajuste del modelo", style = "color: #bbb; font-size: 12px;")
      )
    )
  ),

  dashboardBody(
    useShinyjs(),

    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          min-height: 100vh;
        }
        .box {
          border-radius: 15px;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1);
          backdrop-filter: blur(10px);
          border: 1px solid rgba(255,255,255,0.18);
          transition: all 0.3s ease;
          background: rgba(255,255,255,0.95);
        }
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 15px 45px rgba(0,0,0,0.15);
        }
        .box-header {
          border-radius: 15px 15px 0 0;
          background: linear-gradient(45deg, #667eea, #764ba2);
          color: white;
          padding: 15px 20px;
        }
        .box-header h3:before {
          content: '✨';
          margin-right: 10px;
          font-size: 18px;
        }
        .btn {
          border-radius: 25px;
          padding: 10px 25px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 1px;
          transition: all 0.3s ease;
          border: none;
          position: relative;
          overflow: hidden;
        }
        .btn:before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%;
          width: 100%;
          height: 100%;
          background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
          transition: left 0.5s;
        }
        .btn:hover:before { left: 100%; }
        .btn-primary {
          background: linear-gradient(45deg, #667eea, #764ba2);
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
        }
        .btn-success {
          background: linear-gradient(45deg, #56ab2f, #a8e6cf);
          box-shadow: 0 4px 15px rgba(86, 171, 47, 0.4);
        }
        .form-control {
          border-radius: 12px;
          border: 2px solid #e9ecef;
          padding: 12px 15px;
          transition: all 0.3s ease;
        }
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
          transform: translateY(-1px);
        }
        .control-label {
          font-weight: 600;
          color: #495057;
          margin-bottom: 8px;
          display: flex;
          align-items: center;
        }
        #errorMessages {
          background: linear-gradient(45deg, #ff6b6b, #ee5a52);
          color: white;
          padding: 15px;
          border-radius: 12px;
          margin: 15px 0;
          box-shadow: 0 4px 15px rgba(255, 107, 107, 0.3);
          animation: shake 0.5s ease-in-out;
        }
        @keyframes shake {
          0%, 100% { transform: translateX(0); }
          25% { transform: translateX(-5px); }
          75% { transform: translateX(5px); }
        }
        .fade-in { animation: fadeIn 0.5s ease-in; }
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
      "))
    ),

    tabItems(
      # ======================================================================
      # PESTAÑA 1: CARGAR DATOS
      # ======================================================================
      tabItem(tabName = "load",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📂 Cargar Archivo CSV",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Suba un archivo CSV. El separador se detectará automáticamente.",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        fileInput("csv_file", "Seleccionar archivo CSV",
                                  accept = c(".csv", ".txt")),
                        fluidRow(
                          column(6,
                                 radioButtons("sep_mode", "Separador:",
                                              choices = c("Automático" = "auto",
                                                          "Coma (,)" = ",",
                                                          "Punto y coma (;)" = ";",
                                                          "Tabulación" = "\t"),
                                              selected = "auto", inline = TRUE)
                          )
                        ),
                        div(id = "errorMessages")
                      )
                    )
                  ),
                  fluidRow(
                    box(
                      title = "📋 Vista Previa de Datos",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      withSpinner(DT::DTOutput("data_preview"), type = 5, color = "#667eea")
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 2: ANÁLISIS GAM
      # ======================================================================
      tabItem(tabName = "gam",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📊 Configurar Modelo GAM",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Seleccione las columnas correspondientes en su dataset.",
                          style = "color: #6c757d; margin-bottom: 25px;"),
                        fluidRow(
                          column(4,
                                 selectInput("year_col", "📅 Columna Año:", choices = NULL)
                          ),
                          column(4,
                                 selectInput("doy_col", "🔢 Columna DOY:", choices = NULL)
                          ),
                          column(4,
                                 selectInput("missing_col", "🌫️ Columna con Valores:", choices = NULL)
                          )
                        ),
                        div(style = "text-align: center; margin-top: 30px;",
                            actionButton("run_gam", "🚀 Ejecutar Modelo GAM",
                                         icon = icon("play"),
                                         class = "btn-primary",
                                         style = "width: 80%; font-size: 16px; padding: 15px;")
                        )
                      )
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 3: TABLA DE RESULTADOS
      # ======================================================================
      tabItem(tabName = "results",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📊 Resultados del Modelo GAM",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(class = "results-container",
                          withSpinner(DT::DTOutput("gam_table"), type = 5, color = "#667eea")
                      )
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 4: GRÁFICO
      # ======================================================================
      tabItem(tabName = "plot",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📉 Proporción de Missings y Predicción GAM",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      withSpinner(plotOutput("gam_plot", height = "600px"), type = 4, color = "#667eea")
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 5: AJUSTE DEL MODELO (NUEVA)
      # ======================================================================
      tabItem(tabName = "fit",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📐 Evaluación del Ajuste del Modelo",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Métricas clave que indican qué tan bien el modelo GAM explica el patrón de datos faltantes.",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        verbatimTextOutput("model_summary"),
                        br(),
                        h4("🔍 ¿Cómo interpretar estos valores?", style = "color: #495057;"),
                        tags$ul(
                          tags$li(strong("% Deviance explained:"), "Porcentaje de variabilidad explicada (como R²). >40% = buen ajuste, >60% = excelente."),
                          tags$li(strong("EDF > 1:"), "Indica patrón no lineal (curvo) en la proporción de missings a lo largo del año."),
                          tags$li(strong("p-value < 0.05:"), "El patrón estacional de missings es estadísticamente significativo."),
                          tags$li(strong("AIC bajo:"), "Mejor para comparar modelos (menor es mejor).")
                        ),
                        style = "font-size: 14px; line-height: 1.6;"
                      )
                    )
                  )
              )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  data <- reactiveVal(NULL)
  gam_result <- reactiveVal(NULL)

  # ==========================================================================
  # CARGAR CSV CON DETECCIÓN AUTOMÁTICA
  # ==========================================================================
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

  # ==========================================================================
  # EJECUTAR GAM
  # ==========================================================================
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

  # ==========================================================================
  # TABLA RESULTADOS
  # ==========================================================================
  output$gam_table <- DT::renderDT({
    req(gam_result())
    DT::datatable(
      gam_result()$proporciones,
      options = list(scrollX = TRUE, pageLength = 20),
      rownames = FALSE
    ) %>%
      DT::formatRound(c("Proporcion_Missing", "Predicted"), digits = 4)
  })

  # ==========================================================================
  # GRÁFICO
  # ==========================================================================
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

  # ==========================================================================
  # EVALUACIÓN DEL AJUSTE (NUEVA)
  # ==========================================================================
  output$model_summary <- renderPrint({
    req(gam_result())
    model <- gam_result()$model
    s <- summary(model)

    cat("📐 EVALUACIÓN DEL AJUSTE DEL MODELO\n")
    cat(rep("=", 50), "\n\n")

    cat(sprintf("📈 Deviance explained: %.1f%%\n", 100 * s$dev.expl))
    cat("   → Cuanto más alto, mejor explica el modelo los datos\n\n")

    cat(sprintf("📏 EDF (suavizado DOY): %.2f\n", s$edf[1]))
    cat("   → EDF ≈ 1 = relación lineal | EDF > 1 = patrón curvo/estacional\n\n")

    cat(sprintf("🔍 p-value suavizado DOY: %.2e\n", s$s.pv[1]))
    cat("   → p < 0.05 = patrón estacional significativo\n\n")

    cat(sprintf("⚖️  AIC: %.2f\n", AIC(model)))
    cat("   → Valor más bajo = mejor modelo (para comparar)\n")
  })
}

# ============================================================================
# FUNCIÓN GAM.missing()
# ============================================================================
GAM.missing <- function(data, year_col, doy_col, missing_col) {
  library(dplyr)
  library(mgcv)

  if (missing(year_col) || missing(doy_col) || missing(missing_col)) {
    stop("Debe proporcionar los nombres de columna para year_col, doy_col y missing_col.")
  }
  if (!all(c(year_col, doy_col, missing_col) %in% names(data))) {
    stop("Una o más columnas especificadas no existen en los datos.")
  }

  proporciones <- data %>%
    group_by(!!sym(year_col), !!sym(doy_col)) %>%
    summarise(
      Total = n(),
      Missing = sum(is.na(!!sym(missing_col))),
      Proporcion_Missing = Missing / Total,
      .groups = "drop"
    )

  formula_gam <- as.formula(paste("cbind(Missing, Total - Missing) ~ s(", doy_col, ", bs = 'cs') + ", year_col))
  gam_model <- gam(formula_gam, family = binomial(link = "logit"), data = proporciones)

  proporciones$Predicted <- predict(gam_model, type = "response")

  return(list(proporciones = proporciones, model = gam_model))
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================
shinyApp(ui = ui, server = server)
