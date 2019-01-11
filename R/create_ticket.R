#' @import shiny
#' @import miniUI
NULL

#' Create ticket
#'
#' Create tickets (Title and body) from the ixplorer addin without loosing the
#' ideas during your workflow. tickets will be in the repository that corresponds
#' to the information youo give in the authenticate gadget.
#'
#' @export
create_tickets <- function() {

  ui <- miniPage(
    gadgetTitleBar("Create a new ticket",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniContentPanel(
      verbatimTextOutput("warning", placeholder = FALSE),

      textInput(inputId = "ticket_title",
                label = "ticket title",
                width = "150%",
                placeholder = "Brief description of your ticket"),

      textAreaInput(inputId = "ticket_description",
                    label = "Description",
                    width = "190%",
                    height = "100%",
                    resize = "vertical",
                    rows = 13,
                    placeholder = "Describe the ticket you have encountered")
    ),
    miniButtonBlock(
      actionButton(inputId = "create", label = "Create ticket")
    )
  )

  server <- function(input, output, session) {

    access_file <- verify_ixplorer_file()

    output$warning <- renderText({
      msg <- if (access_file$empty == TRUE) {
        "no credential file available"
      } else {
        set_authentication(access_data = access_file$gitear_access)
      }
      return(msg)
    })

    # ----------------------------------------------------------------
    observeEvent(input$done, {
      gitear::create_issue(base_url = Sys.getenv("IXURL"),
                           api_key = Sys.getenv("IXTOKEN"),
                           owner = Sys.getenv("IXPROJECT"),
                           repo = Sys.getenv("IXREPO"),
                           title = input$ticket_title,
                           body =  input$ticket_description)
      stopApp(NULL)
    })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })

    observeEvent(input$create, {
      tryCatch(
        {
          gitear::create_issue(base_url = Sys.getenv("IXURL"),
                               api_key = Sys.getenv("IXTOKEN"),
                               owner = Sys.getenv("IXPROJECT"),
                               repo = Sys.getenv("IXREPO"),
                               title = input$ticket_title,
                               body =  input$ticket_description)
        },
        error = print("No ticket were created because of invalid credentials. Please use authentication gadget.")
      )

      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
