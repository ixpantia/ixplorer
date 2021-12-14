#' @title List open tickets
#' @description List of open tickets in the repository
#'
#' @param instance instance from ixplorer (E.g. "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in
#'ixplorer
#' @param repository the name of the repository where the tickets are.
#' @param lag is the time in days that you want to look back. For example if
#'you want to see the tickets closed in the last 7 days, lag = 7.
#'By default it shows all the tickets without any lag..
#'
#' @export
list_open_tickets <- function(instance = "saved", lag = NULL) { #no need for other especifications

  # Keyring code to be implemented later --------------------------------------
  # credentials <- tryCatch({
  #   keyrings <- keyring::keyring_list()
  #   error = function(cond) "no_credentials"})
  # if (credenciales() == "no_credentials") {
  #   stop(paste("Aún no existen credenciales para", instance)) # Needs instance
  # }

  # Look for instance ---------------------------------------------------------

  # The default instance value is "saved", so it first looks for a saved
  # keyring in case user forgets to authenticate. It then chooses the last saved
  # keyring.
  # If the user chooses a specific instance other than "saved" ,
  # such as "secure" or "prueba" then that instance is used

  if (instance == "saved"){

    # It looks in session
    if (Sys.getenv("ixplorer_instance") != "") {

      instance <- Sys.getenv("ixplorer_instance")


      # If there is no enviroment variable it means user is looking for
      # a previously saved instance
    } else if (Sys.getenv("ixplorer_instance") == ""){

      saved_instances <- keyring::keyring_list() %>%
        filter(stringr::str_detect(keyring, "ixplorer_"))

      # if there are saved instances, then it chooses the instance that was last saved
      if (nrow(saved_instances) > 0) {

        last_saved <- saved_instances[1,1]
        instance <- last_saved


        # When there are no saved instances, then a message is printed
      } else {
        message("There are no saved instances")
      }

    }

    # If the user chooses an instance other than "saved" then it looks for
    # the specified instance in previously saved keyrings

  } else {

    saved_instances <- keyring::keyring_list() %>%
      select(keyring) %>%
      filter(keyring == paste0("ixplorer_",instance))

    if (nrow(saved_instances) > 0){
      instance <- toString(saved_instances[1])

    } else {
      message("No credentials for ", instance)
    }



  }
  # Keyring llavero ------------------------------------------------------------

  raw_tickets_data <- gitear::get_issues_open_state(
    base_url = keyring::key_get("ixplorer_url", keyring = instance), ## Needs instance
    api_key = keyring::key_get("ixplorer_token", keyring = instance),
    owner = keyring::key_get("ixplorer_project", keyring = instance),
    repo = keyring::key_get("ixplorer_repo", keyring = instance))

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
#' @param instance instancia de ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param owner el nombre del proyecto donde se encuentra el repositorio en
#' ixplorer
#' @param repository el nombre del repositorio donde están los tiquetes
#' @param lag es el tiempo en días que se quiere ver hacia atrás. Por ejemplo si
#' se quiere ver los tiquetes creados en los últimos 7 días, lag = 7. Por
#' defecto muestra todos los tiquetes sin ningún lag.
#'
#' @export
listar_tiquetes_abiertos <- function(instancia = "guardada", dias = NULL){

  if(instancia == "guardada"){

    list_open_tickets(instance = "saved", lag = dias)

  } else {

    list_open_tickets(instance = instancia, lag = dias)
    }

}
