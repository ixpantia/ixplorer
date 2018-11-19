#' @import shiny
#' @import miniUI
NULL

#' Create issue
#'
#' @export
create_issue <- function() {

  ui <- miniPage(
    gadgetTitleBar("Create a new issue",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniContentPanel(
      textInput(inputId = "issue_title",
                label = "Issue title",
                width = "100%",
                placeholder = "Brief description of your issue"),

      textAreaInput(inputId = "issue_description",
                    label = "Description",
                    width = "190%",
                    resize = "vertical",
                    rows = 10,
                    placeholder = "Describe the issue you have encountered")
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp(NULL)
    })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
