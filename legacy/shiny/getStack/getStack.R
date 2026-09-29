library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

# Supongamos que la función get.Stack está definida en otro lugar
# Aquí solo se incluye como un placeholder para la función real
get.Stack <- function(IV_path, QFLAG, output_path) {
  # Simulación de la función para propósitos de demostración
  Sys.sleep(2)  # Simula tiempo de procesamiento
  return(paste("Stacks raster creados en:", output_path))
}

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Get Stack"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Crear Stacks Raster", tabName = "create_stack", icon = icon("stack-overflow"))
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
      # Pestaña para crear stacks raster
      tabItem(tabName = "create_stack",
              fluidRow(
                box(
                  title = "Parámetros para Crear Stacks Raster", status = "primary", solidHeader = TRUE, width = 12,
                  textInput("IV_path", "Ruta de datos VI:"),
                  textInput("QFLAG", "Ruta de datos QFLAG:"),
                  textInput("output_path", "Ruta de salida para los stacks:"),
                  div(id = "errorMessages"),
                  actionButton("create_stack_btn", "Crear Stacks Raster", icon = icon("download"), class = "btn-success")
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

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
