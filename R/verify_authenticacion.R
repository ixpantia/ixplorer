#  Verificar cada uno de los elementos dentro del ixplorer file
verify_authentication <- function(){
  access_data <- verify_ixplorer_file()
  verify_ixurl(access_data)
  verify_token(access_data)
  verify_ixowner(access_data)
  verify_ixrepo(access_data)
  verify_ixuser(access_data)
}

