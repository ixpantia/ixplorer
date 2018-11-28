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
