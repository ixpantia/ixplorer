
#' Añade el upstream del proyecto
#'
#' Valida cual es el repositorio madres del proyecto e incluyelo
#'
#' @export
incluye_upstream <- function() {
  camino <- getwd()
  proyecto_madre = paste0(
    "https://", 
    Sys.getenv("IXURL"), "/",
    Sys.getenv("IXPROJECT"), "/", 
    Sys.getenv("IXREPO"), ".git")

  git2r::remote_add(repo = camino, name = "upstream", url = proyecto_madre)   
}
