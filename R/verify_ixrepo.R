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
