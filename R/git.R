#' @title Add the upstream to the repository that is active in RStudio.
#' @description Add the upstream to the repository that is active in RStudio at
#'   the moment
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in
#'   ixplorer
#'
#' @export
add_upstream <- function(instance = "saved") {

  # Look for instance ---------------------------------------------------------
  if (instance == "saved") {

    # It looks in session
    if (Sys.getenv("ixplorer_instance") != "") {

      instance <- Sys.getenv("ixplorer_instance")
      message ("You are currently on ", instance)

      # If there is no enviroment variable it means user is looking for
      # a previously saved instance
    } else if (Sys.getenv("ixplorer_instance") == "") {

      saved_instances <- keyring::keyring_list() %>%
        filter(stringr::str_detect(keyring, "ixplorer_"))

      # if there are saved instances, then it chooses the instance that was last saved
      if (nrow(saved_instances) > 0) {

        last_saved <- saved_instances[1,1]
        instance <- last_saved
        message("You are currently on ", instance)

        # When there are no saved instances, then a message is printed
      } else {
        message("There are no saved instances, try the authentication gadget")
      }

    }

    # If the user chooses an instance other than "saved" then it looks for
    # the specified instance in previously saved keyrings

  } else {

    saved_instances <- keyring::keyring_list() %>%
      select(keyring) %>%
      filter(keyring == paste0("ixplorer_",instance))

    if (nrow(saved_instances) > 0) {
      instance <- toString(saved_instances[1])

    # If the instance specified was not found a message is printed
   } else {
      message("No credentials for ", instance)
    }
  }

  # Completing the repo url ---------------------------------------------------
  camino <- here::here()

  repository <- basename(rstudioapi::getActiveProject())

  ix_url <- keyring::key_get("ixplorer_url", keyring = instance)

  ix_owner <- keyring::key_get("ixplorer_project", keyring = instance)

  proyecto_madre = paste0(
    ix_url, "/",
    owner, "/",
    repository,
    ".git")

  # adding the remote ---------------------------------------------------------

  git2r::remote_add(repo = camino, name = "upstream", url = proyecto_madre)


  # previous code -------------------------------------------------------------
  # credentials <- tryCatch(
  #   keyring::key_get(paste0("token_", instance)),
  #   error = function(cond) "no_credentials")


  # if (credentials != "no_credentials") {
  #   credentials <- credentials %>%
  #     stringr::str_split("/", simplify = TRUE) %>%
  #     tibble::as_tibble() %>%
  #     magrittr::set_names(c("url", "token",
  #                           "user", "persistence")) %>%
  #     dplyr::mutate(persistence = as.logical(persistence))
  #
  # } else {
  #   stop(paste("No credentials yet for", instance))
  # }
  #
  # if (credentials$persistence == FALSE) {
  #   keyring::key_delete(paste0("token_", instance))
  # }
}


#' Title Agregue el upstream al repositorio que está activo en RStudio.
#'
#' @param instancia instancia de ixplorer (Ej: "secure", "masterclass", "prueba")
#'
#' @details Esta función es la versión en español de add_upstream().
#' Perminte agregar el upstream al repositorio que está activo en RStudio en
#' este momento
#' @return
#' @export
incluye_upstream <- function(instancia = "guardada"){

  if(instancia == "guardada"){
    add_upstream(instance =  "saved")
  } else {
    add_upstream(instance = instancia)
  }

}



#' @title Extend git credential cache time-out
#' @description Set the number of seconds before the git credential cache times
#'   out. Note that this is written for work on remote servers where we do not
#'   want to store our git credentials (other might have acces to it, or
#'   impersonate us). On a trusted machine the most common setting would be ( on
#'   a linux machine) <git credential.helper store>. However, some caching is
#'   required when working on a remote server because frequent commit-push
#'   cycles where you have to write out your username and password every time is
#'   not nice.
#'
#'   By default this will only apply to the repo you are working in. If you call
#'   the global version take care that this is what you want. Also by default
#'   the timeout is set to 4 hours (14400 seconds).
#'
#' @param timeout number of seconds before timeout
#' @param global whether the change should be global or local to the repo
#'
#' @export
set_git_timeout <- function(timeout = 14400, global = FALSE) {

  if (global == TRUE) {
  instruction <- paste0("git config --global credential.helper cache --timeout=", timeout)
  } else {
  instruction <- paste0("git config credential.helper cache --timeout=", timeout)
  }

  system(instruction)
}


#' @title Extienda el tiempo de sus credenciales de git
#' @description Esta es una traducida que llama a set_git_timeout().
#' Establezca la cantidad de segundos para que se agote el
#' tiempo de espera del caché de credenciales de git. Tenga en cuenta que
#' esto está escrito para trabajar en servidores remotos donde no queremos
#' almacenar nuestras credenciales de git (otros pueden tener acceso o
#' hacerse pasar por nosotros). En una máquina confiable, la configuración más
#' común sería (en una máquina Linux) <git credential.helper store>. Sin
#' embargo, se requiere algo de almacenamiento en caché cuando se trabaja en
#' un servidor remoto porque los ciclos frecuentes de envío y confirmación en
#' los que tiene que escribir su nombre de usuario y contraseña cada vez no son
#' agradables.
#'
#' De forma predeterminada, esto solo se aplicará al repositorio en el que está
#' trabajando. Si llama a la versión global, asegúrese de que esto es lo que
#' desea. También por defecto, el tiempo de espera se establece en 4 horas
#' (14400 segundos).
#'
#' @param detencion
#' @param global
#'
#' @export
fijar_pausa_git <- function(pausa =  14000, global = FALSE) {

  if (global == FALSE) {
    set_git_timeout(tiempo = detencion, global = FALSE)
  } else {
    set_git_timeout(tiempo = dentencion, global = TRUE)
  }

}




