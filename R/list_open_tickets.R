#' @title List open tickets
#' @description List of open tickets in the repository
#'
#' @param instance instance from ixplorer (E.g. "secure", "masterclass", "prueba")
#' @param lag is the time in days that you want to look back. For example if
#'you want to see the tickets closed in the last 7 days, lag = 7.
#'By default it shows all the tickets without any lag..
#'
#' @return No return value, called for side effects
#' @export
list_open_tickets <- function(instance = "saved", lag = NULL) {


  # Look for instance ---------------------------------------------------------

  if (instance == "saved") {

    instance <- get_instance()

    if (instance == "none") {
      stop("There are no saved instances")
    }

  } else {

    saved_instances <- keyring::keyring_list() %>%
      select(keyring) %>%
      filter(keyring == paste0("ixplorer_",instance))

    if (nrow(saved_instances) > 0) {

      instance <- toString(saved_instances[1])

    } else {

      stop("No credentials for ", instance)
    }

  }

  # Keyring llavero ------------------------------------------------------------

  raw_tickets_data <- gitear::get_issues_open_state(
    base_url = keyring::key_get("ixplorer_url", keyring = instance), ## Needs instance
    api_key = keyring::key_get("ixplorer_token", keyring = instance),
    owner = keyring::key_get("ixplorer_project", keyring = instance),
    repo = keyring::key_get("ixplorer_repo", keyring = instance))

  if (nrow(raw_tickets_data) == 0) {

    repo <- keyring::key_get("ixplorer_repo", keyring = instance)

    stop("No ticket data found in ", repo)
  }

  # Ticket list ---------------------------------------------------------------
  list <- raw_tickets_data %>%
    mutate(created_at = lubridate::ymd_hms(created_at) -
             lubridate::hours(6),
           updated_at = lubridate::ymd_hms(updated_at) -
             lubridate::hours(6),
           closed_at = lubridate::ymd_hms(closed_at) -
             lubridate::hours(6),
           user.last_login = lubridate::ymd_hms(user.last_login) -
             lubridate::hours(6),
           user.created = lubridate::ymd_hms(user.created) -
             lubridate::hours(6),
           milestone.due_on = lubridate::ymd_hms(milestone.due_on) -
             lubridate::hours(6),
           assignee.last_login = lubridate::ymd_hms(assignee.last_login) -
             lubridate::hours(6),
           assignee.created = lubridate::ymd_hms(assignee.created) -
             lubridate::hours(6)) %>% #Deja hora Costa Rica
    {if (!is.null(lag) == TRUE) {
      dplyr::filter(., created_at >= Sys.Date() - lubridate::days(lag))
    } else {.}} %>%
    dplyr::select(number, title, milestone.title) %>%
    dplyr::arrange(milestone.title, number) %>%
    dplyr::rename(nr = number,
                  Titulo = title,
                  Hito = milestone.title) %>%
    tibble::as_tibble( )

  return(list)
}

#' @title Lista de tiquetes abiertos
#' @description Listado de tiquetes abiertos del repositorio indicado.
#'
#' @param instancia instancia de ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param dias es el tiempo en días que se quiere ver hacia atrás. Por ejemplo si
#' se quiere ver los tiquetes creados en los últimos 7 días, lag = 7. Por
#' defecto muestra todos los tiquetes sin ningún lag.
#'
#' @return No hay valor de retorno - se llama por su efecto secundario
#' @export
listar_tiquetes_abiertos <- function(instancia = "guardada", dias = NULL){

  if (instancia == "guardada") {

    list_open_tickets(instance = "saved", lag = dias)

  } else {

    list_open_tickets(instance = instancia, lag = dias)
    }
}
