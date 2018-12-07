#' @import shiny
#' @import miniUI
#' @import gitear
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

    # Verificar/configurar datos de autentificacion
    access_file <- verify_ixplorer_file()
    set_authentication(access_data = access_file)

    if (Sys.getenv("IXTOKEN") == "") {
      print("no hay IXTOKEN")
    }

    if (Sys.getenv("IXURL") == "") {
      print("no hay IXURL")
    }

    if (Sys.getenv("IXOWNER") == "") {
      print("no hay IXOWNER")
    }

    if (Sys.getenv("IXREPO") == "") {
      print("no hay IXREPO")
    }

    if (Sys.getenv("IXUSER") == "") {
      print("no hay IXUSER")
    }

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
