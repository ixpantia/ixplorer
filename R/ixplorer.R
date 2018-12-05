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
#' Visualize the issues of an specific user, a team and closed issues based on
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
                     tableOutput("team_issues")
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
    ixplorer_user = Sys.getenv("IXUSER")

    # Desanidar cuadro
    issues <- flatten(issues)

    # issues <- rename(issues, Title = title)
    # issues <- rename(issues, Body = body)
    # issues <- rename(issues, `Due Date` = due_date)

    output$my_issues <- function() {
      # Seleccion de issues por usuario y estado abierto
      issues <- issues %>%
        filter(assignee.login == ixplorer_user) %>%
        select(title, body, due_date) %>%
        separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = ymd(due_date) - today())

      issues_kable <- issues %>%
        mutate(due_date =
                 cell_spec(due_date, color = "white", bold = T,
                           background = spec_color(1:nrow(issues), end = 0.9,
                                                   direction = -1))) %>%
        kable(escape = F) %>%
        kable_styling("striped", "condensed")

      return(issues_kable)
    }

    output$team_issues <- function(){
      # Seleccionamos issues por estado abierto
      issues <- issues %>%
        select(user.login, title, body, due_date) %>%
        separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = ymd(due_date) - today())

      issues_kable <- issues %>%
        mutate(due_date =
                 cell_spec(due_date, color = "white", bold = T,
                           background = spec_color(1:nrow(issues), end = 0.9,
                                                   direction = -1)),
               user.login = cell_spec(user.login, bold = ifelse(ixplorer_user == user.login,
                                                   T, F),
                                color = ifelse(ixplorer_user  == user.login,
                                               "gray", "black"))) %>%
        kable(escape = F) %>%
        kable_styling("striped", "condensed")

      return(issues_kable)

    }

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



