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
