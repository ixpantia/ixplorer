library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# Primer paso revisar que existe archivo
verify_ixplorer_file <- function(){
  # Leer ixplorer y poner condicionales -------------------------
  if(file.exists("mytoken.csv")){
    gitear_access <- read_csv("mytoken.csv") %>%
      separate(col = V1, into = c("object", "value"), sep = " ")
  } else {
    gitear_access <- "no acces data"
  }
  return(gitear_access)
}

# Prueba de concepto verificar elementos:
# a <- verify_ixplorer_file()
# verify_ixtoken(a)
# verify_ixurl(a)
# verify_ixowner(a)
# verify_ixrepo(a)
# verify_ixuser(a)

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
  if(TRUE %in% str_detect(gitear_access$object, "IXREPO")){
    entry <- gitear_access %>%
      filter(object == "IXREPO=") %>%
      select(value)
    Sys.setenv("IXREPO" = entry)
  } else {
    print("There is no ixplorer REPOSITORY, please use the Authentication gadget")
  }
}

#  Verificar cada uno de los elementos dentro del ixplorer file

## De esta manera no esta configurando el entorno
set_authentication <- function(){
  access_data <- verify_ixplorer_file()
  verify_ixurl(access_data)
  verify_ixtoken(access_data)
  verify_ixowner(access_data)
  verify_ixrepo(access_data)
  verify_ixuser(access_data)
}

## Segunda manera:
access_data <- verify_ixplorer_file()
set_authentication(access_data)
set_authentication <- function(){
  verify_ixurl(access_data)
  verify_ixtoken(access_data)
  verify_ixowner(access_data)
  verify_ixrepo(access_data)
  verify_ixuser(access_data)
}




