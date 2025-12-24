# Cargar script global
source("global.R")

# ui.R ------------------------------------------------------------------------
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
