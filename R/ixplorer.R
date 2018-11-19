#' @import shiny
#' @import miniUI
#' @import DT
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
      miniTabPanel(title = "Open issues",
                   icon = icon("table"),
                   DT::dataTableOutput("open_issues")
                  ),
      miniTabPanel(title = "Open issues",
                   icon = icon("table"),
                   helpText("open issues")
                  ),
      miniTabPanel(title = "Open issues",
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
      DT::renderDataTable(iris)
    })

    observeEvent(input$done, {
      stopApp(NULL)
    })

  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
