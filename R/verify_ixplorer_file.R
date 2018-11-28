verify_ixplorer_file <- function(){
  # Leer ixplorer y poner condicionales -------------------------
  if(file.exists(".ixplorer")){
    gitear_access <- readLines(".ixplorer")
  } else {
    gitear_access <- "no .ixplorer file"
  }
  return(gitear_access)
}
