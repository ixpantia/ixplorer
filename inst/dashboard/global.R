library(shiny)
library(dplyr)
library(plotly)
library(lubridate)
library(stringr)
library(gitear)


# Set global options ----------------------------------------------------------
## Si pongo estas funciones no sirve el dashboard. da un error

access_file <- ixplorer:::verify_ixplorer_file()

msg <- if(access_file == "no access data"){
  print(access_file)
} else {
  ixplorer:::set_authentication(access_data = access_file)
}
warning(msg)


# Define global functions -----------------------------------------------------

# example data ----------------------------------------------------------------------

  get_data <- function() {

    proyectos <- gitear::get_organizations(
      base_url = Sys.getenv("IXURL"),
      api_key = Sys.getenv("IXTOKEN"))

    repos <- list()
    for (name in proyectos$username) {
      repos[[name]] <- gitear::get_list_repos_org(
        base_url = Sys.getenv("IXURL"),
        api_key = Sys.getenv("IXTOKEN"),
        org = name)
    }

    lista_proyectos <- list()
    for (repo in names(repos)) {
      lista_proyectos[[repo]] <- repos[[repo]]$name
    }

    return(lista_proyectos)
  }

  get_projects <- function(ixplorer_data) {
    proyectos <- names(ixplorer_data)
    return(proyectos)
  }
# Load modules ----------------------------------------------------------------
source("modules/project.R", encoding = "UTF-8")
source("modules/repository.R", encoding = "UTF-8")
