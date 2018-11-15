#' @import shiny
#' @import miniUI
NULL

#' ixplorer miniUI
#'
#' @export
ixplorer <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer",
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniTabstripPanel(
      miniTabPanel(title = "open_issues",
                   icon = icon("table"),
                   helpText("open issues")
                  ),
      miniTabPanel(title = "closed issies",
                   icon = icon("table"),
                   helpText("open issues")
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp(NULL)
    })

  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
