#' @import tidyr
#' @import dplyr
#' @import readr
#' @import stringr
NULL

#' Verify authentication to ixplorer
#'
verify_ixplorer_file <- function(){
  # Leer ixplorer y poner condicionales -------------------------
  if(file.exists(".ixplorer")){
    gitear_access <- read_csv(".ixplorer") %>%
      separate(col = V1, into = c("object", "value"), sep = " ")
  } else {
    gitear_access <- "no access data"
  }
  return(gitear_access)
}

    ## IXTOKEN ----
verify_ixtoken <- function(gitear_access){
  if(TRUE %in% str_detect(gitear_access$object, "IXTOKEN")){
    entry <- gitear_access %>%
      filter(object == "IXTOKEN=") %>%
      select(value)
    Sys.setenv("IXTOKEN" = entry)
  } else {
    print("There is no ixplorer TOKEN, please use the Authentication gadget")
  }
}

    ## IXURL ----
verify_ixurl <- function(gitear_access){
  if(TRUE %in% str_detect(gitear_access$object, "IXURL")){
    entry <- gitear_access %>%
      filter(object == "IXURL=") %>%
      select(value)
    Sys.setenv("IXURL" = entry)
  } else {
    print("There is no ixplorer URL, please use the Authentication gadget")
  }
}

  ## IXOWNER ----
verify_ixowner <- function(gitear_access){
  if(TRUE %in% str_detect(gitear_access$object, "IXOWNER")){
    entry <- gitear_access %>%
      filter(object == "IXOWNER=") %>%
      select(value)
    Sys.setenv("IXOWNER" = entry)
  } else {
    print("There is no ixplorer PROJECT name, please use the Authentication gadget")
  }
}

    ## IXREPO ----
verify_ixrepo <- function(gitear_access){
  if(TRUE %in% str_detect(gitear_access$object, "IXREPO")){
    entry <- gitear_access %>%
      filter(object == "IXREPO=") %>%
      select(value)
    Sys.setenv("IXREPO" = entry)
  } else {
    print("There is no ixplorer REPOSITORY, please use the Authentication gadget")
  }
}

    ## IXUSER ----
verify_ixuser <- function(gitear_access){
  if(TRUE %in% str_detect(gitear_access$object, "IXUSER")){
    entry <- gitear_access %>%
      filter(object == "IXUSER=") %>%
      select(value)
    Sys.setenv("IXUSER" = entry)
  } else {
    print("There is no ixplorer USER, please use the Authentication gadget")
  }
}

#  Verificar cada uno de los elementos dentro del ixplorer file
set_authentication <- function(access_data){
  ixurl <- verify_ixurl(access_data)
  ixtoken <- verify_ixtoken(access_data)
  ixowner <- verify_ixowner(access_data)
  ixrepo <- verify_ixrepo(access_data)
  ixuser <- verify_ixuser(access_data)
  msj <- c(ixurl, ixtoken, ixowner, ixrepo, ixuser)
  print(msj[!msj == "TRUE"])
}

