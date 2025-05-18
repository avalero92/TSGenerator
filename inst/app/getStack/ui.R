library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

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
