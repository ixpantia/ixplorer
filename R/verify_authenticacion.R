#' @import dplyr
NULL

#' Verify authentication to ixplorer
#'
#' Verify if there is  a .ixplorer file in your working directory and set
#' the variables.
#'
#'
verify_ixplorer_file <- function(){
  api_creds <- list()
  working_directory <- rstudioapi::getActiveProject()
  ixplorer_file <- paste0(working_directory, "/.ixplorer")
  # Leer ixplorer y poner condicionales -------------------------
  if (file.exists(ixplorer_file)) {
    readRenviron(ixplorer_file)
    #gitear_access <- readr::read_csv(ixplorer_file) %>%

    conn <- file(ixplorer_file, open = "r")
    lines <- readLines(conn)
    close(conn)

    gitear_access <- as.data.frame(lines)
    api_creds$empty <- FALSE
    api_creds$gitear_access <- tidyr::separate(gitear_access, lines,
                                     into = c("variable", "value"), sep = "=")

  } else {
    api_creds$empty <- TRUE
  }
  return(api_creds)
}

#' Verify  ixtoken
#'
#' Verify if there is a token to your ixplorer repository
#'
verify_ixtoken <- function(gitear_access){
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXTOKEN") &&
        FALSE %in% any(is.na(gitear_access[1,2])))) {
    print("There is no ixplorer TOKEN, please use the Authentication gadget")
  }
}

#' Verify  ixurl
#'
#' Verify if there is an URL to your ixplorer repository
#'
verify_ixurl <- function(gitear_access){
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXURL") &&
        FALSE %in% any(is.na(gitear_access[2,2])))) {
        print("There is no ixplorer URL, please use the Authentication gadget")
  }
}

  ## IXPROJECT ----
#' Verify  ixproject
#'
#' Verify if there is a project name where your ixplorer repository belongs.
#'
verify_ixproject <- function(gitear_access){
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXPROJECT") &&
     FALSE %in% any(is.na(gitear_access[3,2])))) {
        print("There is no ixplorer PROJECT name, please use the Authentication gadget")
  }
}

    ## IXREPO ----
#' Verify  ixrepo
#'
#' Verify if there is the name of your ixplorer repository
#'
verify_ixrepo <- function(gitear_access){
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXREPO") &&
      FALSE %in% any(is.na(gitear_access[4,2])))) {
    print("There is no ixplorer REPOSITORY, please use the Authentication gadget")
  }
}

    ## IXUSER ----
#' Verify  ixuser
#'
#' Verify if there is an user name.
#'
verify_ixuser <- function(gitear_access) {
  if (!(TRUE %in% stringr::str_detect(gitear_access$variable, "IXUSER") &&
     FALSE %in% any(is.na(gitear_access[5,2])))) {
       print("There is no ixplorer USER, please use the Authentication gadget")
  }
}

#' Verify each of the steps
#'
#' Verify  each of the elements needed to access your repository from a gadget
#'
#'
set_authentication <- function(access_data) {
  ixurl <- verify_ixurl(access_data)
  ixtoken <- verify_ixtoken(access_data)
  ixproject <- verify_ixproject(access_data)
  ixrepo <- verify_ixrepo(access_data)
  ixuser <- verify_ixuser(access_data)
  msj <- c(ixurl, ixtoken, ixproject, ixrepo, ixuser)
  return(msj)
}

