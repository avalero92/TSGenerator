library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Renames Times Series VI"),
  
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