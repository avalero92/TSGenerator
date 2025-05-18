library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

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
