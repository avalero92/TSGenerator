# ============================================================================
# Renames_Image_IV_App - Interfaz Shiny para renames.image.IV()
# Estilo idéntico a tus apps anteriores
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyFiles)
library(fs)

# ============================================================================
# UI
# ============================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-edit", style = "margin-right: 8px;"),
      "Rename Times Series VI",
      style = "font-weight: bold; font-size: 18px;"
    ),
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("⚙️ Configuración",
               tabName = "config",
               icon = icon("cog"),
               badgeLabel = "Primero",
               badgeColor = "orange"),
      menuItem("🖼️ Renombrar Imágenes",
               tabName = "rename",
               icon = icon("edit"),
               badgeLabel = "Principal",
               badgeColor = "blue"),
      menuItem("📊 Resultados",
               tabName = "results",
               icon = icon("chart-bar"),
               badgeLabel = "Resumen",
               badgeColor = "green"),
      br(),
      div(style = "padding: 15px;",
          h5("💡 Consejos rápidos", style = "color: #fff; font-weight: bold;"),
          p("• Archivos deben contener fecha YYYYMMDD", style = "color: #bbb; font-size: 12px;"),
          p("• Ej: S2A_..._20230115_...", style = "color: #bbb; font-size: 12px;"),
          p("• Se renombrará a 20230115.tif", style = "color: #bbb; font-size: 12px;")
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
        .results-container {
          background: rgba(255,255,255,0.95);
          border-radius: 15px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        }
        .fade-in {
          animation: fadeIn 0.5s ease-in;
        }
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
      "))
    ),

    tabItems(
      # ======================================================================
      # PESTAÑA 1: CONFIGURACIÓN
      # ======================================================================
      tabItem(tabName = "config",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📂 Seleccionar Carpeta de Imágenes",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Seleccione la carpeta que contiene los archivos .tif con nombres largos (que incluyen fecha en formato YYYYMMDD).",
                          style = "color: #6c757d; margin-bottom: 20px;"),
                        fluidRow(
                          column(12,
                                 div(class = "form-group",
                                     tags$label("📁 Carpeta con imágenes TIF:", class = "control-label"),
                                     shinyDirButton("input_folder", "Seleccionar carpeta", "Elija la carpeta con imágenes"),
                                     verbatimTextOutput("input_path_display")
                                 )
                          )
                        )
                      )
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 2: RENOMBRAR IMÁGENES
      # ======================================================================
      tabItem(tabName = "rename",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "🖼️ Renombrar Serie Temporal VI",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(
                        p("Esta herramienta renombrará todos los archivos .tif de la carpeta seleccionada usando solo la fecha (YYYYMMDD).",
                          style = "color: #6c757d; margin-bottom: 25px;"),
                        p("Ejemplo: S2A_MSIL2A_20230115T105231_... → 20230115.tif",
                          style = "font-style: italic; color: #495057;"),
                        div(id = "errorMessages"),
                        div(style = "text-align: center; margin-top: 30px;",
                            actionButton("run_rename", "🚀 Renombrar Imágenes",
                                         icon = icon("edit"),
                                         class = "btn-primary",
                                         style = "width: 80%; font-size: 16px; padding: 15px;")
                        )
                      )
                    )
                  )
              )
      ),

      # ======================================================================
      # PESTAÑA 3: RESULTADOS
      # ======================================================================
      tabItem(tabName = "results",
              div(class = "fade-in",
                  fluidRow(
                    box(
                      title = "📊 Resumen del Renombrado",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      div(class = "results-container",
                          withSpinner(
                            verbatimTextOutput("rename_summary"),
                            type = 5,
                            color = "#667eea"
                          )
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

  # ==========================================================================
  # SELECTOR DE CARPETA CON TODOS LOS DISCOS LOCALES
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

  # ==========================================================================
  # VALORES REACTIVOS PARA RESULTADOS
  # ==========================================================================
  values <- reactiveValues(
    renamed_count = 0,
    log_messages = character(),
    last_folder = NULL
  )

  # ==========================================================================
  # FUNCIÓN renames.image.IV (integrada y mejorada)
  # ==========================================================================
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

  # ==========================================================================
  # EJECUTAR RENOMBRADO
  # ==========================================================================
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

  # ==========================================================================
  # RESUMEN DE RESULTADOS
  # ==========================================================================
  output$rename_summary <- renderPrint({
    cat("📊 RESUMEN DEL RENOMBRADO - TIMES SERIES VI\n")
    cat(rep("=", 55), "\n\n")

    if (is.null(values$last_folder)) {
      cat("📭 Aún no se ha ejecutado ningún renombrado.\n\n")
      cat("👉 Vaya a la pestaña 'Renombrar Imágenes' y haga clic en 'Renombrar Imágenes'\n")
    } else {
      cat("📁 Carpeta procesada:\n   ", values$last_folder, "\n\n")
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

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================
shinyApp(ui = ui, server = server)
