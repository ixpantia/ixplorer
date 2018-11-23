#' @import shiny
#' @import miniUI
#' @import DT
#' @import gitear
#' @import dplyr
#' @import jsonlite
NULL

#' ixplorer reports
#'
#' Visualize issues of an specific user, a team and closed issues based on
#' the credentials used in gadget authenticate.
#'
#' @export
ix_reports <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer Reports"),
    miniTabstripPanel(
      miniTabPanel("My issues", icon = icon("user"),
                   miniContentPanel(
                     DT::dataTableOutput("my_issues")
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
    issues <- gitear::get_issues(base_url = Sys.getenv("IXURL"),
                                 api_key = Sys.getenv("IXTOKEN"),
                                 owner = Sys.getenv("IXOWNER"),
                                 repo = Sys.getenv("IXREPO"))
    user = Sys.getenv("IXUSER")

    # Desanidar cuadro
    issues <- flatten(issues)

    output$my_issues <- DT::renderDataTable({
      # Seleccion de issues por usuario y estado abierto
      issues <- issues %>%
        filter(assignee.login == user) %>%
        filter(state == "open") %>%
        select(title, body,due_date, milestone, labels)
      return(issues)
    })

    output$team_issues <- DT::renderDataTable({
      # Seleccionamos issues por estado abierto
      issues <- issues %>%
        filter(state == "open") %>%
        select(title, body,due_date, milestone, labels)

      return(issues)
    })

    output$closed_issues <- DT::renderDataTable({
      # Traer issues que estan cerrados. TODO
      #
      # issues <- issues %>%
      #   filter(state == "close") %>%
      #   select(title, body,due_date, milestone, labels)
      #
      # return(issues)
      iris

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



