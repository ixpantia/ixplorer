#' @title Añade el upstream al repositorio que está activo en RStudio
#' @description Añade el upstream al repositorio que está activo en RStudio en
#' este momento
#'
#' @param instance instancia de ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param owner el nombre del proyecto donde se encuentra el repositorio en
#' ixplorer
#'
#' @export
incluye_upstream <- function(instance, owner) {

  camino <- here::here()

  repository <- basename(rstudioapi::getActiveProject())

  credenciales <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credenciales")


  if(credenciales != "no_credenciales") {
    credenciales <- credenciales %>%
      stringr::str_split("/", simplify = TRUE) %>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("url", "token",
                            "usuario", "persistencia")) %>%
      dplyr::mutate(persistencia = as.logical(persistencia))

  } else {
    stop(paste("Aún no existen credenciales para", instance))
  }

  if(credenciales$persistencia == FALSE) {
    keyring::key_delete(paste0("token_", instance))
  }

  proyecto_madre = paste0(
    "https://",
    credenciales$url, "/",
    owner, "/",
    repository,
    ".git")

  git2r::remote_add(repo = camino, name = "upstream", url = proyecto_madre)
}
