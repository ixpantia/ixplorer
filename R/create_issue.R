library(miniUI)

create_issue <- function() {

  ui <- miniPage(
    gadgetTitleBar("Create a new issue"),
    miniContentPanel(
      textInput(inputId = "issue_title", "Issue title", width = "100%",
                placeholder = "Brief description of your issue"),
      textAreaInput(inputId = "issue_description", "Description",
                    width = "190%", resize = "vertical",
                    rows = 10,
                    placeholder = "Describe the issue you have encountered")
    )
  )

  server <- function(input, output, session) {


    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
