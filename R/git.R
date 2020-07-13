
#' Añade el upstream del proyecto
#'
#' Valida cual es el repositorio madres del proyecto e incluyelo
#'
#' @export
incluye_upstream <- function(instance, owner, repository = "current") {

  camino <- here::here()

  if(repository == "current") {
    repository <- basename(rstudioapi::getActiveProject())
  }

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

  proyecto_madre = paste0(
    "https://",
    credenciales$url, "/",
    owner, "/",
    repository,
    ".git")

  git2r::remote_add(repo = camino, name = "upstream", url = proyecto_madre)
}
