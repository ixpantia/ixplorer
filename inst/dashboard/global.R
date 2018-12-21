library(shiny)
# Set global options ----------------------------------------------------------

# Define global functions -----------------------------------------------------

# example data ----------------------------------------------------------------------

  get_data <- function() {
    lista_proyectos <- list()
    lista_proyectos$primero <- c("uno", "dos")
    lista_proyectos$segundo <- c("hola", "hello", "goodbye")
    return(lista_proyectos)
  }

  get_projects <- function(ixplorer_data) {
    proyectos <- names(ixplorer_data)
    return(proyectos)
  }
# Load modules ----------------------------------------------------------------
source("modules/project.R", encoding = "UTF-8")
source("modules/repository.R", encoding = "UTF-8")
