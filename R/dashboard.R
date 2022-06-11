#' Show ixplorer dashboard
#' @return No return value, called for side effects
#' @export
dashboard <- function() {
  app_directory <- system.file("dashboard", package = "ixplorer")

  shiny::runApp(app_directory)
}


