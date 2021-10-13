#' @import shiny
#' @import miniUI
NULL

#' @title Authentication in ixplorer
#' @description Saves to your computer
#' through the authentication system of your computer's OS.
#'
#' @details In case the credentials already exist,
#' just enter the url and confirm if you want to keep the credentials
#' on your computer or want to delete them after the next query.
#'
#' @export
add_token <- function() {

  ui <- miniPage(
    gadgetTitleBar("Ixplorer authentication",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),
    miniContentPanel(
      textInput(inputId = "ixplorer_url",
                label = "ixplorer URL",
                width = "100%",
                placeholder = "Copy your ixplorer URL here."),
      uiOutput("token_user"),
      checkboxInput(inputId = "token_persist",
                    value = 1,
                    label = "Persistence of credentials on this computer.
                    (Do not use on shared computers)",
                    width = "100%"
      )
    )
  )

  server <- function(input, output, session) {

    output$token_user <- renderUI({
      req(input$ixplorer_url)

      instancia <- sub("\\..*", "", input$ixplorer_url)

      verifica_cred <- tryCatch(
        keyring::key_get(paste0("token_", instancia)),
        error = function(cond) "no_credenciales")

      if(verifica_cred == "no_credenciales") {
          div(textInput(inputId = "ixplorer_token",
                    label = "Access Token",
                    width = "100%",
                    placeholder = "Enter your Access Token here"),
              textInput(inputId = "ixplorer_user_name",
                        label = "Your username.",
                        width = "100%",
                        placeholder = "Enter your username here."))
      }

    })

    observeEvent(input$done, {

      instancia <- sub("\\..*", "", input$ixplorer_url)

      verifica_cred <- tryCatch(
        keyring::key_get(paste0("token_", instancia)),
        error = function(cond) "no_credenciales")


      if(verifica_cred == "no_credenciales") {

        if(is.null(input$ixplorer_url) == FALSE |
           is.null(input$ixplorer_token) == FALSE |
           is.null(input$ixplorer_user_name) == FALSE) {

          keyring::key_set_with_value(
            service = paste0("token_", instancia),
            password = paste(input$ixplorer_url,
                             input$ixplorer_token,
                             input$ixplorer_user_name,
                             input$token_persist,
                             sep = "/"))
        }

      } else {

        }

        stopApp(TRUE)
      })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(TRUE)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
