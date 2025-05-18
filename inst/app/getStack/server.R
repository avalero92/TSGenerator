library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga


# Lógica del servidor
server <- function(input, output, session) {
  
  # Crear stacks raster
  observeEvent(input$create_stack_btn, {
    output$result <- renderPrint({
      tryCatch({
        # Validar entradas
        if (input$IV_path == "" || input$QFLAG == "" || input$output_path == "") {
          stop("Por favor, complete todos los campos.")
        }
        
        # Llamar a la función get.Stack
        result <- get.Stack(input$IV_path, input$QFLAG, input$output_path)
        result
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })
  })
}

