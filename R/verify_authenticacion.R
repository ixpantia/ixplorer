#' @import dplyr
NULL

#' Verify authentication to ixplorer
#'
#' Verify if there is  a .ixplorer file in your working directory and set
#' the variables.
#'
verify_ixplorer_file <- function(){
  # Leer ixplorer y poner condicionales -------------------------
  if(file.exists(".ixplorer")){
    gitear_access <- readr::read_csv(".ixplorer") %>%
      tidyr::separate(col = V1, into = c("object", "value"), sep = " ")
  } else {
    gitear_access <- "no access data"
  }
  return(gitear_access)
}

## IXTOKEN ----

#' Verify  ixtoken
#'
#' Verify if there is a token to your ixplorer repository
#'
verify_ixtoken <- function(gitear_access){
  if(TRUE %in% stringr::str_detect(gitear_access$object, "IXTOKEN")){
    entry <- gitear_access %>%
      filter(object == "IXTOKEN=") %>%
      select(value)
    Sys.setenv("IXTOKEN" = entry)
  } else {
    print("There is no ixplorer TOKEN, please use the Authentication gadget")
  }
}

    ## IXURL ----
#' Verify  ixurl
#'
#' Verify if there is an URL to your ixplorer repository
#'
verify_ixurl <- function(gitear_access){
  if(TRUE %in% stringr::str_detect(gitear_access$object, "IXURL")){
    entry <- gitear_access %>%
      filter(object == "IXURL=") %>%
      select(value)
    Sys.setenv("IXURL" = entry)
  } else {
    print("There is no ixplorer URL, please use the Authentication gadget")
  }
}

  ## IXPROJECT ----
#' Verify  ixproject
#'
#' Verify if there is a project name where your ixplorer repository belongs.
#'
verify_ixproject <- function(gitear_access){
  if(TRUE %in% stringr::str_detect(gitear_access$object, "IXOWNER")){
    entry <- gitear_access %>%
      filter(object == "IXOWNER=") %>%
      select(value)
    Sys.setenv("IXOWNER" = entry)
  } else {
    print("There is no ixplorer PROJECT name, please use the Authentication gadget")
  }
}

    ## IXREPO ----
#' Verify  ixrepo
#'
#' Verify if there is the name of your ixplorer repository
#'
verify_ixrepo <- function(gitear_access){
  if(TRUE %in% stringr::str_detect(gitear_access$object, "IXREPO")){
    entry <- gitear_access %>%
      filter(object == "IXREPO=") %>%
      select(value)
    Sys.setenv("IXREPO" = entry)
  } else {
    print("There is no ixplorer REPOSITORY, please use the Authentication gadget")
  }
}

    ## IXUSER ----
#' Verify  ixuser
#'
#' Verify if there is an user name.
#'
verify_ixuser <- function(gitear_access){
  if(TRUE %in% stringr::str_detect(gitear_access$object, "IXUSER")){
    entry <- gitear_access %>%
      filter(object == "IXUSER=") %>%
      select(value)
    Sys.setenv("IXUSER" = entry)
  } else {
    print("There is no ixplorer USER, please use the Authentication gadget")
  }
}

#' Verify  each of the elements needed to access your repository from a gadget
#'
#' Verify each of the steps
#'
set_authentication <- function(access_data){
  verify_ixurl(access_data)
  verify_ixtoken(access_data)
  verify_ixproject(access_data)
  verify_ixrepo(access_data)
  verify_ixuser(access_data)
}

