library(shiny)
library(reticulate)
library(magick)
library(shinydashboard)
library(shinyjs) 
library(shinycssloaders)

# Carga las definiciones de ui.R y server.R
source("ui.R")
source("server.R")

# Ejecuta la aplicación Shiny
shinyApp(ui = ui, server = server)