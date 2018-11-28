#  Verificar cada uno de los elementos dentro del ixplorer file
verify_authentication <- function(){
  access_data <- verify_ixplorer_file()
  verify_ixurl(access_data)
  verify_token(access_data)
  verify_ixowner(access_data)
  verify_ixrepo(access_data)
  verify_ixuser(access_data)
}

verify_ixplorer_file <- function(){
  # Leer ixplorer y poner condicionales -------------------------
  if(file.exists("mytoken.csv")){
    gitear_access <- read_csv("mytoken.csv")
  } else {
    gitear_access <- "no acces data"
  }
  return(gitear_access)
}

# Prueba de concepto verificar elementos:

a <- verify_ixplorer_file()
a <- separate(data = a, col = V1, into = c("object", "value"), sep = " ")

verify_ixtoken <- function(gitear_access){
  if(TRUE %in% str_detect(a$object, "IXURL")){
    entry <- a %>%
      filter(object == "IXTOKEN=") %>%
      select(value)
    Sys.setenv("IXTOKEN" = entry)
  } else {
    print("There is no ixplorer TOKEN, please use the Authentication gadget")
  }
}
verify_ixurl <- function(gitear_access){
  # IXURL ------
  if(TRUE %in% str_detect(gitear_access, "IXURL")){
    # separar el url de ixtoken asegurandonos que entre el ixtoken
    intake <- str_subset(gitear_access, "IXURL") %>%
      str_split("=", simplify = TRUE)
    intake <- intake[1,2]
    # tomar ese elemento y unirlo en el Sys.setenv
    Sys.setenv("IXURL" = intake)
    # Dar mensaje de error si no hay intake
  } else {
    print("There is no ixplorer URL, please use  the Authentication gadget")
  }
}


verify_ixuser <- function(gitear_access){
  # IXUSER ------
  if(TRUE %in% str_detect(gitear_access, "IXUSER")){
    # separar el repo de ixrepo asegurandonos que entre el ixrepo
    intake <- str_subset(gitear_access, "IXUSER") %>%
      str_split("=", simplify = TRUE)
    intake <- intake[1,2]
    # tomar ese elemento y unirlo en el Sys.setenv
    Sys.setenv("IXUSER" = intake)
    # Dar mensaje de error si no hay intake
  } else {
    print("There is no ixplorer user name, please use  the Authentication gadget")
  }
}

verify_token <- function(gitear_access){
  # IXTOKEN ------
  if(TRUE %in% str_detect(gitear_access, "IXTOKEN")){
    # separar el token de ixtoken asegurandonos que entre el ixtoken
    intake <- str_subset(gitear_access, "IXTOKEN") %>%
      str_split("=", simplify = TRUE)
    intake <- intake[1,2]
    # tomar ese elemento y unirlo en el Sys.setenv
    Sys.setenv("IXTOKEN" = intake)
    # Dar mensaje de error si no hay intake
  } else {
    print("There is no ixplorer token, please use  the Authentication gadget")
  }
}

verify_ixrepo <- function(gitear_access){
  # IXUREPO ------
  if(TRUE %in% str_detect(gitear_access, "IXREPO")){
    # separar el repo de ixrepo asegurandonos que entre el ixrepo
    intake <- str_subset(gitear_access, "IXREPO") %>%
      str_split("=", simplify = TRUE)
    intake <- intake[1,2]
    # tomar ese elemento y unirlo en el Sys.setenv
    Sys.setenv("IXREPO" = intake)
    # Dar mensaje de error si no hay intake
  } else {
    print("There is no ixplorer repository name, please use  the Authentication gadget")
  }
}


verify_ixowner <- function(gitear_access){
  # IXOWNER ------
  if(TRUE %in% str_detect(gitear_access, "IXOWNER")){
    # separar el repo de ixrepo asegurandonos que entre el ixrepo
    intake <- str_subset(gitear_access, "IXOWNER") %>%
      str_split("=", simplify = TRUE)
    intake <- intake[1,2]
    # tomar ese elemento y unirlo en el Sys.setenv
    Sys.setenv("IXOWNER" = intake)
    # Dar mensaje de error si no hay intake
  } else {
    print("There is no ixplorer project name, please use  the Authentication gadget")
  }
}
