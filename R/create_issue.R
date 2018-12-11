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
      a <- if(access_file == "no access data"){
        print(access_file)
      } else {
        set_authentication(access_data = access_file)
      }

      b <- if (Sys.getenv("IXTOKEN") == "") {
        print("no hay IXTOKEN")
      }

      c <- if (Sys.getenv("IXURL") == "") {
        print("no hay IXURL")
      }

      d <- if (Sys.getenv("IXOWNER") == "") {
        print("no hay IXOWNER")
      }

      e <- if (Sys.getenv("IXREPO") == "") {
        print("no hay IXREPO")
      }

      f <- if (Sys.getenv("IXUSER") == "") {
        print("no hay IXUSER")
      }

      text <- paste("Warning! There is no:", a, b, c, d, e, f)
      return(text())
    })

    # Verificar/configurar datos de autentificacion



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
