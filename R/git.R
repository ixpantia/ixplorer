#' @title Add the upstream to the repository that is active in RStudio.
#' @description Add the upstream to the repository that is active in RStudio at
#'   the moment
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in
#'   ixplorer
#'
#' @export
add_upstream <- function(instance, owner) {

  camino <- here::here()

  repository <- basename(rstudioapi::getActiveProject())

  credentials <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credentials")


  if (credentials != "no_credentials") {
    credentials <- credentials %>%
      stringr::str_split("/", simplify = TRUE) %>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("url", "token",
                            "user", "persistence")) %>%
      dplyr::mutate(persistence = as.logical(persistence))

  } else {
    stop(paste("No credentials yet for", instance))
  }

  if (credentials$persistence == FALSE) {
    keyring::key_delete(paste0("token_", instance))
  }

  proyecto_madre = paste0(
    "https://",
    credentials$url, "/",
    owner, "/",
    repository,
    ".git")

  git2r::remote_add(repo = camino, name = "upstream", url = proyecto_madre)
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
