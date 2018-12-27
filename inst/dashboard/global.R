library(shiny)
library(dplyr)
library(plotly)
library(lubridate)
library(stringr)
library(gitear)
# library(ixplorer)

# Set global options ----------------------------------------------------------
## Si pongo estas funciones no sirve el dashboard. da un error
access_file <- verify_ixplorer_file()
# set_authentication(access_data = access_file)

# Define global functions -----------------------------------------------------

# example data ----------------------------------------------------------------------

  get_data <- function() {
    proyectos <- gitear::get_list_repos_org(
      base_url = Sys.getenv("IXURL"),
      api_key = Sys.getenv("IXTOKEN"),
      org = Sys.getenv("IXPROJECT"))
    lista_proyectos <- list()
    lista_proyectos$ixplorer <- proyectos$name
    lista_proyectos$project_2 <- c("hola", "hello", "goodbye")
    return(lista_proyectos)
  }

  get_projects <- function(ixplorer_data) {
    proyectos <- names(ixplorer_data)
    return(proyectos)
  }
# Load modules ----------------------------------------------------------------
source("modules/project.R", encoding = "UTF-8")
source("modules/repository.R", encoding = "UTF-8")
