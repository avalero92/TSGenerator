# global.R --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyFiles)
library(fs)

# Detección de todos los discos locales para shinyFiles
volumes <- c()
if (.Platform$OS.type == "windows") {
  drives <- sapply(LETTERS[1:26], function(l) {
    drive <- paste0(l, ":/")
    if (dir.exists(drive)) drive else NULL
  })
  volumes <- unlist(drives)
  names(volumes) <- paste0(volumes, " ")
} else {
  volumes <- c("Raíz (/)" = "/", "Home" = fs::path_home())
}
