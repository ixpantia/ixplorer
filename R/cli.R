#' @import dplyr
NULL


#' list open tickets
#'
#' list tickets. the "current" respository is the one linked to the active
#' RStudio project.
#'
#' @export
list_open_tickets <- function(lag = 7, repository = "current") {
  access_file <- ixplorer:::verify_ixplorer_file()

  if (access_file$empty == TRUE) {
      warning("no credential file available")
    } else {
      ixplorer:::set_authentication(access_data = access_file$gitear_access)
    }

    list <-  gitear::get_issues_open_state(
             base_url = Sys.getenv("IXURL"),
             api_key = Sys.getenv("IXTOKEN"),
             owner = Sys.getenv("IXPROJECT"),
             repo = Sys.getenv("IXREPO"))

    list <- list %>%
      select(number, title, milestone.title) %>%
      arrange(milestone.title, number) %>%
      rename(nr = number,
        Titulo = title,
        Hito = milestone.title)

    return(list)
}

#' list closed tickets
#'
#' list tickets. the "current" respository is the one linked to the active
#' RStudio project.
#'
#' @export
list_closed_tickets <- function(lag = 7, repository = "current") {

  access_file <- ixplorer:::verify_ixplorer_file()

  if (access_file$empty == TRUE) {
      warning("no credential file available")
    } else {
      ixplorer:::set_authentication(access_data = access_file$gitear_access)
    }

    list <-  gitear::get_issues_closed_state(
             base_url = Sys.getenv("IXURL"),
             api_key = Sys.getenv("IXTOKEN"),
             owner = Sys.getenv("IXPROJECT"),
             repo = Sys.getenv("IXREPO"))

    list <- list %>%
      select(number, title, milestone.title) %>%
      arrange(milestone.title, number) %>%
      rename(nr = number,
        Titulo = title,
        Hito = milestone.title)

    return(list)
}
