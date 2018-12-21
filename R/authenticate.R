#' @import shiny
#' @import miniUI
NULL

#' Authenticate to ixplorer
#'
#' Make the connection to your repository through the ixplorer gadget and be
#' able to create issues, review issues without re-writing your credentials
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
                label = "The API token",
                width = "100%",
                placeholder = "Paste your ixplorer token here"),

      textInput(inputId = "ixplorer_url",
                label = "ixplorer url",
                width = "100%",
                placeholder = "Paste your ixplorer url here"),

      textInput(inputId = "ixplorer_project_name",
                label = "The name of the upstream project",
                width = "100%",
                placeholder = "Paste your ixplorer upstream project name here"),

      textInput(inputId = "ixplorer_repo_name",
                label = "ixplorer repository name",
                width = "100%",
                placeholder = "Paste your ixplorer repository name here"),

      textInput(inputId = "ixplorer_user_name",
                label = "Your ixplorer user name",
                width = "100%",
                placeholder = "Paste your ixplorer user name here"),

      checkboxInput(inputId = "token_persist",
                    value = 0,
                    label = "Persist token? (do no use on shared computer)",
                    width = "100%"
      )

    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {

      Sys.setenv("IXTOKEN" = input$ixplorer_token)
      Sys.setenv("IXURL"   = input$ixplorer_url)
      Sys.setenv("IXPROJECT" = input$ixplorer_project_name)
      Sys.setenv("IXREPO"  = input$ixplorer_repo_name)
      Sys.setenv("IXUSER"  = input$ixplorer_user_name)

      token <- paste("IXTOKEN=", input$ixplorer_token, sep = " ")
      url   <- paste("IXURL=", input$ixplorer_url, sep = " ")
      project <- paste("IXPROJECT=", input$ixplorer_project_name, sep = " ")
      repo  <- paste("IXREPO=", input$ixplorer_repo_name, sep = " ")
      user  <- paste("IXUSER=", input$ixplorer_user_name, sep = " ")

      access_data <- rbind(token, url, project, repo, user)
      access_data <- as.data.frame(access_data)

      if (input$token_persist == 1) {
        readr::write_csv(access_data, col_names = TRUE, path = ".ixplorer")
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
