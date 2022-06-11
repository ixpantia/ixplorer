#' @import dplyr
NULL

#' Verificar autentificacion del ixplorer.
#'
#' Verifique si hay un archivo .ixplorer en el directorio de trabajo y
#' configure las variables.
#'
verify_ixplorer_file <- function() {
  api_creds <- list()
  working_directory <- rstudioapi::getActiveProject()
  ixplorer_file <- paste0(working_directory, "/.ixplorer")
  # Leer ixplorer y poner condicionales -------------------------
  if (Sys.getenv("IXTOKEN") != "" &
      Sys.getenv("IXURL") != "" &
      Sys.getenv("IXPROJECT") != "" &
      Sys.getenv("IXREPO")  != "" &
      Sys.getenv("IXUSER")  != "") {

    token   <- paste0("IXTOKEN=", Sys.getenv("IXTOKEN"))
    url     <- paste0("IXURL=", Sys.getenv("IXURL"))
    project <- paste0("IXPROJECT=", Sys.getenv("IXPROJECT"))
    repo    <- paste0("IXREPO=", Sys.getenv("IXREPO"))
    user    <- paste0("IXUSER=", Sys.getenv("IXUSER"))

    lines <- c(token, url, project, repo, user)
    lines <- as.data.frame(lines)
    api_creds$empty <- FALSE
    api_creds$gitear_access <- tidyr::separate(lines, lines,
                                               into = c("variable", "value"),
                                               sep = "=")

  } else if (file.exists(ixplorer_file)) {
    readRenviron(ixplorer_file)
    conn <- file(ixplorer_file, open = "r")
    lines <- readLines(conn)
    close(conn)

    gitear_access <- as.data.frame(lines)
    api_creds$empty <- FALSE
    api_creds$gitear_access <- tidyr::separate(
                                  gitear_access,
                                  lines,
                                  into = c("variable", "value"), sep = "=")

  } else {
    api_creds$empty <- TRUE
  }
  return(api_creds)
}

#' @title Verifies ixplorer user
#' @description Verifies token exists for a repository.
#'
#' @param gitear_access parameters to access into an ixplorer instance

verify_ixtoken <- function(gitear_access) {
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXTOKEN") &&
        FALSE %in% any(is.na(gitear_access[1, 2])))) {
    print("No hay TOKEN de ixplorer, por favor use el gadget de
          autentificacion")
  }
}

#' @title Verifies ixplorer url
#' @description Verifies if the url exists in the repository
#'
#' @param gitear_access parameters to access into an ixplorer instance

verify_ixurl <- function(gitear_access) {
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXURL") &&
        FALSE %in% any(is.na(gitear_access[2, 2])))) {
    print("No hay URL de ixplorer, por favor use el gadget de autentificacion")
  }
}

#' @title Verifies ixplorer project
#' @description Verifies if the project's name exists in the repository.
#'
#' @param gitear_access parameters to access into an ixplorer instance

verify_ixproject <- function(gitear_access) {
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXPROJECT") &&
        FALSE %in% any(is.na(gitear_access[3, 2])))) {
    print("No hay NOMBRE del proyecto, por favor use el gadget de
          autentificacion")
  }
}

#' @title Verifies ixplorer repo
#' @description Verifies if ixplorer repository's name exists.
#'
#' @param gitear_access parameters to access into an ixplorer instance

verify_ixrepo <- function(gitear_access) {
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXREPO") &&
        FALSE %in% any(is.na(gitear_access[4, 2])))) {
    print("No hay repositorio ixplorer, por favor use el gadget de
          autentificacion")
  }
}

#' @title Verifies ixplorer user
#' @description Verifies if user name exists.
#'
#' @param gitear_access parameters to access into an ixplorer instance

verify_ixuser <- function(gitear_access) {
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXUSER") &&
        FALSE %in% any(is.na(gitear_access[5, 2])))) {
    print("No hay usuario ixplorer, por favor use el gadget de autentificacion")
  }
}

#' @title Set authentication
#' @description Set the authentification into ixplorer
#'
#' @param access_data Data needed to get into ixplorer
#' @export

set_authentication <- function(access_data) {
  ixurl <- verify_ixurl(access_data)
  ixtoken <- verify_ixtoken(access_data)
  ixproject <- verify_ixproject(access_data)
  ixrepo <- verify_ixrepo(access_data)
  ixuser <- verify_ixuser(access_data)
  msj <- c(ixurl, ixtoken, ixproject, ixrepo, ixuser)
  return(msj)
}

