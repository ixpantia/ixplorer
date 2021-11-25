#' @title List closed tickets
#' @description List of closed tickets from the indicated repository.
#'
#' @param instance instance from ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in
#'ixplorer
#' @param repository the name of the repository where the issues are.
#' @param lag is the time in days that you want to look back. For example if
#'you want to see the issues closed in the last 7 days, lag = 7.
#'By default it shows all the issues without any lag..
#' @export
list_closed_tickets <- function(#instance, owner, unused due to credential hadling
                                # repository = "current",
                                lag = NULL) {

  # Keyring code to be implemented late ---------------------------------------
  # credenciales <- tryCatch(
  #   keyring::key_get(paste0("token_", instance)),
  #   error = function(cond) "no_credenciales")
  #
  # if(credenciales == "no_credenciales") {
  #   stop(paste("Aún no existen credenciales para", instance))
  # }
  #
  # credenciales <- credenciales %>%
  #   stringr::str_split("/", simplify = TRUE) %>%
  #   tibble::as_tibble() %>%
  #   magrittr::set_names(c("url", "token",
  #                         "usuario", "persistencia")) %>%
  #   dplyr::mutate(persistencia = as.logical(persistencia))
  #
  # if(credenciales$persistencia == FALSE) {
  #   keyring::key_delete(paste0("token_", instance))
  # }

  # Repo from Rstudio API------------------------------------------------------
  # if(repository == "current") {
  #   repository <- basename(rstudioapi::getActiveProject())
  # }
  # Read credentials from .ixplorer TEMPORAL-----------------------------------
  # access_file <- ixplorer:::verify_ixplorer_file()
  # ixplorer_url <- Sys.getenv("IXURL")
  #
  # credentials <- tibble::tribble(
  #   ~url, ~token, ~user, ~owner,
  #   Sys.getenv("IXURL"), Sys.getenv("IXTOKEN"), Sys.getenv("IXUSER"), Sys.getenv("IXPROJECT")
  # )
  #
  # instance <- sub("\\..*", "", ixplorer_url)

  # Creates list with OLD keyring code---------------------------------------------
  # list <-  gitear::get_issues_closed_state(
  #   base_url = paste0("https://", credenciales$url),
  #   api_key = credenciales$token,
  #   owner = owner,
  #   repo = repository)

  #Temporal list---------------------------------------------------------------
  # list <-  gitear::get_issues_closed_state(
  #   base_url = credentials$url,
  #   api_key = credentials$token,
  #   owner = credentials$owner,
  #   repo = repository)

  instance = Sys.getenv("ixplorer_instance") # variable to check workflow

  # Keyring llavero ------------------------------------------------------------
  raw_tickets_data <- gitear::get_issues_open_state(
    base_url = key_get("ixplorer_url", keyring = instance), ## Needs instance
    api_key = key_get("ixplorer_token", keyring = instance),
    owner = key_get("ixplorer_project", keyring = instance),
    repo = key_get("ixplorer_repo", keyring = instance))

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
    {if (!is.null(lag)== TRUE) {
      dplyr::filter(., closed_at >= Sys.Date() - lubridate::days(lag))
    } else {.}} %>%
    dplyr::select(number, title, milestone.title) %>%
    dplyr::arrange(milestone.title, number) %>%
    dplyr::rename(nr = number, #traducciones
                  Titulo = title,
                  Hito = milestone.title) %>%
    tibble::as_tibble()

  return(list)
}
#' @title Lista de tiquetes cerrados
#' @description Listado de tiquetes cerrados del repositorio indicado.
#'
#' @param instance instancia de ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param owner el nombre del proyecto donde se encuentra el repositorio en
#' ixplorer
#' @param repository el nombre del repositorio donde están los tiquetes
#' @param lag es el tiempo en días que se quiere ver hacia atrás. Por ejemplo si
#' se quiere ver los tiquetes cerrados en los últimos 7 días, lag = 7. Por
#' defecto muestra todos los tiquetes sin ningún lag.
#'
#' @export
listar_tiquetes_cerrados <- function(...){
  list_closed_tickets()
}
