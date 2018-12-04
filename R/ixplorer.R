#' @import shiny
#' @import miniUI
#' @import DT
#' @import gitear
#' @import dplyr
#' @import jsonlite
#' @import kableExtra
#' @import lubridate
#' @import tidyr
NULL

#' ixplorer reports
#'
#' Visualize issues of an specific user, a team and closed issues based on
#' the credentials used in gadget authenticate.
#'
#' @export
ix_issues <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer Reports"),
    miniTabstripPanel(
      miniTabPanel("My issues", icon = icon("user"),
                   miniContentPanel(
                     tableOutput("my_issues")
                   )
      ),
      miniTabPanel("Team issues", icon = icon("users"),
                   miniContentPanel(
                     DT::dataTableOutput("team_issues")
                   )
      ),
      miniTabPanel("Closed issues", icon = icon("times-circle"),
                   miniContentPanel(
                     DT::dataTableOutput("closed_issues")
                   )
      )

    )
  )

  server <- function(input, output, session){

    # Traemos issues y configuramos credenciales
    issues <- gitear::get_issues_open_state(base_url = Sys.getenv("IXURL"),
                                 api_key = Sys.getenv("IXTOKEN"),
                                 owner = Sys.getenv("IXOWNER"),
                                 repo = Sys.getenv("IXREPO"))
    user = Sys.getenv("IXUSER")

    # Desanidar cuadro
    issues <- flatten(issues)

    output$my_issues <- function() {
      # Seleccion de issues por usuario y estado abierto
      issues <- issues %>%
        filter(assignee.login == user) %>%
        select(title, body, due_date, labels) %>%
        separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = ymd(due_date) - today())

      issues_kable <- issues %>%
        mutate(due_date =
                 cell_spec(due_date, color = "white", bold = T,
                           background = spec_color(1:3, end = 0.9,
                                                   direction = -1))) %>%
        kable(escape = F) %>%
        kable_styling("striped", "condensed")

      return(issues_kable)
    }

    output$team_issues <- DT::renderDataTable({
      # Seleccionamos issues por estado abierto
      issues <- issues %>%
        filter(state == "open") %>%
        select(title, body,due_date, milestone, labels)
      return(issues)
    })

    output$closed_issues <- DT::renderDataTable({
      # Traer issues que estan cerrados. TODO
      issues_closed <- gitear::get_issues_closed_state(base_url = Sys.getenv("IXURL"),
                                        api_key = Sys.getenv("IXTOKEN"),
                                        owner = Sys.getenv("IXOWNER"),
                                        repo = Sys.getenv("IXREPO")
      ) %>%
        select(title, body, due_date, labels)
      issues_closed <- flatten(issues_closed)
      return(issues_closed)
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    observeEvent(input$cancel, {
      stopApp(TRUE)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))

}



