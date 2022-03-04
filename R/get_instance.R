#' Get the active instance in the session
#'
#' @description By default, functions that get tickets and pull requests
#' use the active instance in the session. This functions gets this
#' active instance.
#'
#' @return a character of the active issue in the session
#' @export
#'
get_instance <- function() {


  # Look in session
  if (Sys.getenv("ixplorer_instance") != "") {

    instance <- Sys.getenv("ixplorer_instance")

    # No active instance in session
  } else if (Sys.getenv("ixplorer_instance") == "") {

    saved_instances <- keyring::keyring_list() %>%
      filter(stringr::str_detect(keyring, "ixplorer_"))

    # Choose last saved instance
    if (nrow(saved_instances) > 0) {

      last_saved <- saved_instances[1,1]
      instance <- last_saved

      # No instance found
    } else {

      instance <- "none"

      }
    }

  return(instance )

}
