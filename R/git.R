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

    instance <- get_instance()

    if (instance == "none") {
      stop("There are no saved instances. Try the authentication gadget")
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

}


#' Title Agregue el upstream al repositorio que está activo en RStudio.
#'
#' @param instancia instancia de ixplorer (Ej: "secure", "masterclass", "prueba")
#'
#' @details Esta función es la versión en español de add_upstream().
#' Perminte agregar el upstream al repositorio que está activo en RStudio en
#' este momento
#' @export
incluye_upstream <- function(instancia = "guardada") {

  if (instancia == "guardada") {
    add_upstream(instance =  "saved")
  } else {
    add_upstream(instance = instancia)
  }
}


#' @title Extend git credential cache time-out
#'
#' @description Set the number of seconds before the git credential cache times
#'   out. Note that this is written for work on remote servers where we do not
#'   want to store our git credentials because other might have access to it, or
#'   impersonate us. On a trusted machine the most common setting would be (on
#'   a linux machine) git credential.helper store. However, some caching is
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
    instruction <- paste0('git config --global credential.helper --replace-all "cache --timeout=',
                          timeout, '"')
  } else {
    instruction <- paste0('git config credential.helper "cache --timeout=',
                          timeout, '"')
  }

  system(instruction)
}


#' @title Extienda el tiempo de sus credenciales de git
#'
#' @description Esta es una función traducida que llama a set_git_timeout.
#' Establezca la cantidad de segundos para que se agote el
#' tiempo de espera del caché de credenciales de git. Tenga en cuenta que
#' esto está escrito para trabajar en servidores remotos donde no queremos
#' almacenar nuestras credenciales de git (otros pueden tener acceso o
#' hacerse pasar por nosotros). En una máquina confiable, la configuración más
#' común sería (en una máquina Linux) git credential.helper store. Sin
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
#' @param detencion número de segundos antes del tiempo de espera
#' @param global si el cambio debe ser global o local para el repositorio
#'
#' @export
fijar_tiempo_credenciales <- function(pausa =  14400, global = FALSE) {

  if (global == FALSE) {
    set_git_timeout(timeout = pausa, global = FALSE)
  } else {
    set_git_timeout(timeout = pausa, global = TRUE)
  }
}

