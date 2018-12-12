#' Show ixplorer dashboard
#' 
#' @export
dashboard <- function() {
  app_directory <- system.file("dashboard", package = "ixplorer")

  shiny::runApp(app_directory)
}


