library(shiny)
library(shinydashboard)

# Set global options ----------------------------------------------------------

# Define global functions -----------------------------------------------------

# Load modules ----------------------------------------------------------------
source("modules/sidebar.R", encoding = "UTF-8")
source("modules/project.R", encoding = "UTF-8")
source("modules/repository.R", encoding = "UTF-8")

lista_proyectos <- list()
lista_proyectos$primero <- c("uno", "dos")
lista_proyectos$segundo <- c("hola", "hello", "goodbye")
proyectos <- names(lista_proyectos)

proyectos <- names(lista_proyectos)
repositorios <- c()

for (proyecto in proyectos) {
  repositorio <- unname(unlist(lista_proyectos[proyecto]))
  repositorios <- c(repositorios, repositorio)
}

bodies <- c(proyecto, repositorios)
bodies <- make.names(bodies)
