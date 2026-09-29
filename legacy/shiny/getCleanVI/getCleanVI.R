library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga
library(raster)  # Para trabajar con archivos raster

# Definición de la función get.Clean.IV
get.Clean.IV <- function(stack_folder = NULL, output_folder) {
  if (is.null(stack_folder)) {
    message("Please insert an address to rastestack")
    stop("Address to rastestack is required")
  }

  # Obtener la lista de archivos raster en la carpeta
  raster_files <- list.files(path = stack_folder, pattern = ".tif$", full.names = TRUE)

  # Crear el directorio de salida si no existe
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Iterar sobre cada archivo raster
  for (file in raster_files) {
    # Abrir el raster multibanda
    r <- raster::stack(file)

    # Obtener la matriz de la banda "QFLAG2"
    banda2 <- r[[2]]

    # Aplicar condición para eliminar píxeles en la primera banda
    r[[1]][raster::values(banda2) != 1] <- NA

    # Obtener el nombre del archivo sin la ruta
    file_name <- basename(file)

    # Crear la ruta de salida para el archivo modificado
    output_file <- file.path(output_folder, file_name)

    # Guardar el archivo modificado en la carpeta de salida
    raster::writeRaster(r[[1]], filename = output_file, format = "GTiff", overwrite = TRUE)
  }
}

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Clean Time Series VI"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Limpiar Raster", tabName = "clean_raster", icon = icon("eraser"))
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        #errorMessages {
          color: #dc3545;
          margin: 10px 0;
        }
      "))
    ),

    tabItems(
      # Pestaña para limpiar archivos raster
      tabItem(tabName = "clean_raster",
              fluidRow(
                box(
                  title = "Parámetros para Limpiar Raster", status = "primary", solidHeader = TRUE, width = 12,
                  textInput("stack_folder", "Ruta de la carpeta de archivos raster:"),
                  textInput("output_folder", "Ruta de salida para los archivos limpios:"),
                  div(id = "errorMessages"),
                  actionButton("clean_raster_btn", "Limpiar Archivos Raster", icon = icon("check"), class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Resultado", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(verbatimTextOutput("result"))
                )
              )
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {

  # Limpiar archivos raster
  observeEvent(input$clean_raster_btn, {
    output$result <- renderPrint({
      tryCatch({
        # Validar entradas
        if (input$stack_folder == "" || input$output_folder == "") {
          stop("Por favor, complete todos los campos.")
        }

        # Llamar a la función get.Clean.IV
        get.Clean.IV(input$stack_folder, input$output_folder)
        "Archivos raster limpiados con éxito."
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
