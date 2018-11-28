# Authentication data verification

verify_ixplorer_file <- function(){
  # Leer ixplorer y poner condicionales -------------------------
  if(file.exists(".ixplorer")){
    gitear_access <- readLines(".ixplorer")
  } else {
    gitear_access <- "no .ixplorer file"
  }
  return(gitear_access)
}

verify_authentication <- function(gitear_access){
  # IXTOKEN ------
  if(str_detect(gitear_access, "IXTOKEN")){
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

  ### Esto se detiene aqui por la logica de los ifelse. Por eso no
  ### continua con los demas. Hay que corregir la manera de escribir
  ### el codigo o hacer cada una una funcion o bien un for loop

  # IXURL ------
  if(str_detect(gitear_access, "IXURL")){
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

  # IXOWNER ------
  if(str_detect(gitear_access, "IXOWNER")){
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

  # IXUREPO ------
  if(str_detect(gitear_access, "IXREPO")){
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

  # IXUSER ------
  if(str_detect(gitear_access, "IXUSER")){
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

