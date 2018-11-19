#' @import shiny
#' @import miniUI
NULL

#' Authenticate to ixplorer
#'
#' @export
add_token <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer authentication",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniContentPanel(

      textInput(inputId = "ixplorer_token",
                label = "Your ixplorer token",
                width = "100%",
                placeholder = "Paste your ixplorer token here"),

      checkboxInput(inputId = "token_persist",
                    value = 0,
                    label = "Persist token? (do no use on shared computer)"
                    )

    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {

      Sys.setenv("IXTOKEN" = input$ixplorer_token)

      if (input$token_persist == 1) {
        write(input$token_persist, file = ".ixplorer")
        write(".ixplorer", file = ".gitignore", append = TRUE)
      }

      stopApp(NULL)
    })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
