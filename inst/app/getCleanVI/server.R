library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga
library(raster)  # Para trabajar con archivos raster

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
