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
