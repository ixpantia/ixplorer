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

    # Verificar/configurar datos de autentificacion
    access_file <- verify_ixplorer_file()
    set_authentication(access_data = access_file)

    if (Sys.getenv("IXTOKEN") == "") {
      stop("no hay IXTOKEN")
    }

    if (Sys.getenv("IXURL") == "") {
      stop("no hay IXURL")
    }

    if (Sys.getenv("IXOWNER") == "") {
      stop("no hay IXOWNER")
    }

    if (Sys.getenv("IXREPO") == "") {
      stop("no hay IXREPO")
    }

    if (Sys.getenv("IXUSER") == "") {
      stop("no hay IXUSER")
    }

    if(str_detect(authentication, "There is no"))

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
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
