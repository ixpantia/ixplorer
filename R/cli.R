
#' Enlistar tiquetes abiertos
#'
#' Listado de los tiquetes. El repositorio actual es el que se encuentra
#' ligado al proyecto activo en RStudio.
#'
#' @export
list_open_tickets <- function(instance, owner, repository = "current",
                              lag = 7) {

  credenciales <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credenciales")

  if(credenciales == "no_credenciales") {
    stop(paste("Aún no existen credenciales para", instance))
    }


  if(repository == "current") {
    repository <- basename(rstudioapi::getActiveProject())
  }

  credenciales <- credenciales %>%
    stringr::str_split("/", simplify = TRUE) %>%
    tibble::as_tibble() %>%
    magrittr::set_names(c("url", "token",
                          "usuario", "persistencia")) %>%
    dplyr::mutate(persistencia = as.logical(persistencia))

  if(credenciales$persistencia == FALSE) {
    keyring::key_delete(paste0("token_", instance))
  }

    list <-  gitear::get_issues_open_state(
      base_url = paste0("https://", credenciales$url),
      api_key = credenciales$token,
      owner = owner,
      repo = repository)

    list <- list %>%
      dplyr::select(number, title, milestone.title) %>%
      dplyr::arrange(milestone.title, number) %>%
      dplyr::rename(nr = number,
        Titulo = title,
        Hito = milestone.title) %>%
      tibble::as_tibble()

    return(list)
}

#' Enlistar tiquetes cerrados
#'
#' Listado de los tiquetes. El repositorio actual es el que se encuentra
#' ligado al proyecto activo en RStudio.
#'
#' @export
list_closed_tickets <- function(instance, owner, repository = "current",
                                lag = 7) {

  credenciales <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credenciales")

  if(credenciales == "no_credenciales") {
    stop(paste("Aún no existen credenciales para", instance))
  }


  if(repository == "current") {
    repository <- basename(rstudioapi::getActiveProject())
  }

  credenciales <- credenciales %>%
    stringr::str_split("/", simplify = TRUE) %>%
    tibble::as_tibble() %>%
    magrittr::set_names(c("url", "token",
                          "usuario", "persistencia")) %>%
    dplyr::mutate(persistencia = as.logical(persistencia))

  if(credenciales$persistencia == FALSE) {
    keyring::key_delete(paste0("token_", instance))
  }

  list <-  gitear::get_issues_closed_state(
    base_url = paste0("https://", credenciales$instancia),
    api_key = credenciales$token,
    owner = owner_repository,
    repo = repository)

  list <- list %>%
    dplyr::select(number, title, milestone.title) %>%
    dplyr::arrange(milestone.title, number) %>%
    dplyr::rename(nr = number,
           Titulo = title,
           Hito = milestone.title) %>%
    tibble::as_tibble()

  return(list)
}
