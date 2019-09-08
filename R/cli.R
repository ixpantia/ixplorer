#' @import dplyr
NULL


#' Enlistar tiquetes abiertos
#'
#' Listado de los tiquetes. El repositorio actual es el que se encuentra
#' ligado al proyecto activo en RStudio.
#'
#' @export
list_open_tickets <- function(lag = 7, repository = "current", clip = TRUE) {
  access_file <- ixplorer.es:::verify_ixplorer_file()

  if (access_file$empty == TRUE) {
      warning("No hay archivo de credenciales disponible")
    } else {
      ixplorer.es:::set_authentication(access_data = access_file$gitear_access)
    }

    list <-  gitear::get_issues_open_state(
             base_url = paste("https://", strsplit(Sys.getenv("IXURL"), "//")[[1]][2]),
             api_key = Sys.getenv("IXTOKEN"),
             owner = Sys.getenv("IXPROJECT"),
             repo = Sys.getenv("IXREPO"))

    list <- list %>%
      select(number, title, milestone.title) %>%
      arrange(milestone.title, number) %>%
      rename(nr = number,
        Titulo = title,
        Hito = milestone.title)

    if(clip) {
      clipr::write_clip(list, breaks = "\n")
    }

    return(list)
}

#' Enlistar tiquetes cerrados
#'
#' Listado de los tiquetes. El repositorio actual es el que se encuentra
#' ligado al proyecto activo en RStudio.
#'
#' @export
list_closed_tickets <- function(lag = 7, repository = "current", clip = TRUE) {

  access_file <- ixplorer.es:::verify_ixplorer_file()

  if (access_file$empty == TRUE) {
      warning("No hay archivo de credenciales disponible")
    } else {
      ixplorer.es:::set_authentication(access_data = access_file$gitear_access)
    }

    list <-  gitear::get_issues_closed_state(
             base_url = paste("https://", strsplit(Sys.getenv("IXURL"), "//")[[1]][2]),
             api_key = Sys.getenv("IXTOKEN"),
             owner = Sys.getenv("IXPROJECT"),
             repo = Sys.getenv("IXREPO"))

    list <- list %>%
      select(number, title, milestone.title) %>%
      arrange(milestone.title, number) %>%
      rename(nr = number,
        Titulo = title,
        Hito = milestone.title)

    if(clip) {
      clipr::write_clip(list, breaks = "\n")
    }

    return(list)
}
