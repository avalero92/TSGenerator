library(shiny)
library(shinydashboard)  # Para mejorar la UI
library(shinyjs)  # Para mejorar la funcionalidad de JavaScript
library(shinycssloaders)  # Para spinners de carga
library(raster)  # Para trabajar con archivos raster

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