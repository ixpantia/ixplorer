#' @title Add the upstream to the repository that is active in RStudio.
#' @description Add the upstream to the repository that is active in RStudio
#' at the moment
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in
#' ixplorer
#'
#' @export
add_upstream <- function(instance, owner) {

  camino <- here::here()

  repository <- basename(rstudioapi::getActiveProject())

  credentials <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credentials")


  if(credentials != "no_credentials") {
    credentials <- credentials %>%
      stringr::str_split("/", simplify = TRUE) %>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("url", "token",
                            "user", "persistence")) %>%
      dplyr::mutate(persistence = as.logical(persistence))

  } else {
    stop(paste("No credentials yet for", instance))
  }

  if(credentials$persistence == FALSE) {
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
