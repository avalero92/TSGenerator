library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

# Definición de la función renames.image.IV
renames.image.IV <- function(input_folder = NULL) {
  # Listar los archivos TIF en la carpeta
  archivos <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

  # Verificar si hay archivos para renombrar
  if (length(archivos) == 0) {
    stop("No se encontraron archivos TIF en la carpeta especificada.")
  }

  # Recorrer los archivos y renombrarlos
  for (archivo in archivos) {
    # Extraer la fecha dentro del nombre de la imagen (formato yyyy)
    date <- gsub(".*(\\d{8}).*", "\\1", basename(archivo)) # Para renombrar archivos con el formato de fecha 'yyyymmdd'.

    # Nuevo nombre del archivo con la fecha
    new.name <- file.path(input_folder, paste0(date, ".tif"))

    # Renombrar el archivo
    file.rename(archivo, new.name)
  }
}

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Renames VPP product"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Renombrar Imágenes", tabName = "rename_images", icon = icon("file-image-o"))
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
      # Pestaña para renombrar imágenes
      tabItem(tabName = "rename_images",
              fluidRow(
                box(
                  title = "Parámetros para Renombrar Imágenes", status = "primary", solidHeader = TRUE, width = 12,
                  textInput("input_folder", "Ruta de la carpeta de imágenes TIF:"),
                  div(id = "errorMessages"),
                  actionButton("rename_images_btn", "Renombrar Imágenes", icon = icon("check"), class = "btn-success")
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

  # Renombrar imágenes
  observeEvent(input$rename_images_btn, {
    output$result <- renderPrint({
      tryCatch({
        # Validar entrada
        if (input$input_folder == "") {
          stop("Por favor, complete el campo de la ruta de la carpeta.")
        }

        # Llamar a la función renames.image.IV
        renames.image.IV(input$input_folder)
        "Imágenes renombradas con éxito."
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
