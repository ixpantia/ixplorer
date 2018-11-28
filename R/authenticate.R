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

      textInput(inputId = "ixplorer_url",
                label = "Your ixplorer url",
                width = "100%",
                placeholder = "Paste your ixplorer url here"),

      textInput(inputId = "ixplorer_repo_name",
                label = "Your ixplorer repository name",
                width = "100%",
                placeholder = "Paste your ixplorer repository name here"),

      textInput(inputId = "ixplorer_repo_owner",
                label = "Your ixplorer repository owner",
                width = "100%",
                placeholder = "Paste your ixplorer repository owner here"),

      textInput(inputId = "ixplorer_user_name",
                label = "Your ixplorer user name",
                width = "100%",
                placeholder = "Paste your ixplorer user name here"),

      checkboxInput(inputId = "token_persist",
                    value = 0,
                    label = "Persist token? (do no use on shared computer)"
      )

    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {

      Sys.setenv("IXTOKEN" = input$ixplorer_token)
      Sys.setenv("IXURL"   = input$ixplorer_url)
      Sys.setenv("IXOWNER" = input$ixplorer_repo_owner)
      Sys.setenv("IXREPO"  = input$ixplorer_repo_name)
      Sys.setenv("IXUSER"  = input$ixplorer_user_name)

      # Formatear con paste hacer objeto fuera del write y ese objeto
      # ponerlo dentro del write TODO

      # a <- paste("hola", "\n", "todos")
      # cat(a)

      token <- paste("IXTOKEN=", input$ixplorer_token, sep = "")
      url   <- paste("IXURL=", input$ixplorer_url, sep = "")
      owner <- paste("IXOWNER=", input$ixplorer_repo_owner, sep = "")
      repo  <- paste("IXREPO=", input$ixplorer_repo_name, sep = "")
      user  <- paste("IXUSER=", input$ixplorer_user_name, sep = "")

      if (input$token_persist == 1) {
        write(x = c(token, url, repo, owner), file = ".ixplorer")
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
