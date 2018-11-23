#' @import shiny
#' @import miniUI
#' @import DT
#' @import gitear
#' @import dplyr
#' @import jsonlite
NULL

#' ixplorer reports
#'
#' @export
ix_reports <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer reports",
                   left = miniTitleBarCancelButton(inputId = "full_screen",
                                                   label = "Full Screen",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniTabstripPanel(
      miniTabPanel(title = "My Open issues",
                   icon = icon("table"),
                   tableOutput("open_issues")
      ),
      miniTabPanel(title = "Team issues",
                   icon = icon("table"),
                   helpText("open issues")
      ),
      miniTabPanel(title = "Closed issues",
                   icon = icon("table"),
                   helpText("open issues")
      ),
      miniTabPanel(title = "closed issues",
                   icon = icon("table"),
                   helpText("open issues")
      )
    )
  )

  server <- function(input, output, session) {

    output$open_issues({
      renderTable(iris)
    })

    observeEvent(input$done, {
      stopApp(NULL)
    })

  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
