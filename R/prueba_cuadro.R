#' @import shiny
#' @import miniUI
#' @import DT
#' @import gitear
#' @import dplyr
#' @import jsonlite
NULL

#' ixplorer reportes
#'
#' @export
ix_reportes <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer Reports"),
    miniTabstripPanel(
      miniTabPanel("My issues", icon = icon("sliders"),
                   miniContentPanel(
                     DT::dataTableOutput("my_issues")
                   )
      ),
      miniTabPanel("Team issues", icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("team_issues")
                   )
      )
    )
  )

  server <- function(input, output, session){

    output$my_issues <- DT::renderDataTable({
      issues <- gitear::get_issues(base_url = Sys.getenv("IXURL"),
                                   api_key = Sys.getenv("IXTOKEN"),
                                   owner = Sys.getenv("IXOWNER"),
                                   repo = Sys.getenv("IXREPO"))

      user = Sys.getenv("IXUSER")
      issues <- flatten(issues)

      issues <- issues %>%
        filter(assignee.login == user) %>%
        filter(state == "open") %>%
        select(title, body,due_date, milestone, labels)

      return(issues)
    })

    output$team_issues <- DT::renderDataTable({
      iris
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))

}



