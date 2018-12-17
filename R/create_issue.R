#' @import shiny
#' @import miniUI
NULL

#' Create issue
#'
#' Create issues (Title and body) from the ixplorer addin without loosing the
#' ideas during your workflow. Issues will be in the repository that corresponds
#' to the information youo give in the authenticate gadget.
#'
#' @export
create_issues <- function() {

  ui <- miniPage(
    gadgetTitleBar("Create a new issue",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniContentPanel(
      verbatimTextOutput("warning", placeholder = FALSE),

      textInput(inputId = "issue_title",
                label = "Issue title",
                width = "150%",
                placeholder = "Brief description of your issue"),

      textAreaInput(inputId = "issue_description",
                    label = "Description",
                    width = "190%",
                    height = "100%",
                    resize = "vertical",
                    rows = 13,
                    placeholder = "Describe the issue you have encountered")
    ),
    miniButtonBlock(
      actionButton(inputId = "create", label = "Create Issue")
    )
  )

  server <- function(input, output, session) {

    access_file <- verify_ixplorer_file()

    output$warning <- renderText({

    msg <- if(access_file == "no access data"){
        print(access_file)
      } else {
        set_authentication(access_data = access_file)
      }
      return(msg)
    })

    # ----------------------------------------------------------------
    observeEvent(input$done, {
      gitear::create_issue(base_url = Sys.getenv("IXURL"),
                           api_key = Sys.getenv("IXTOKEN"),
                           owner = Sys.getenv("IXOWNER"),
                           repo = Sys.getenv("IXREPO"),
                           title = input$issue_title,
                           body =  input$issue_description)
      stopApp(NULL)
    })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })

    observeEvent(input$create, {
      gitear::create_issue(base_url = Sys.getenv("IXURL"),
                           api_key = Sys.getenv("IXTOKEN"),
                           owner = Sys.getenv("IXOWNER"),
                           repo = Sys.getenv("IXREPO"),
                           title = input$issue_title,
                           body =  input$issue_description)
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
