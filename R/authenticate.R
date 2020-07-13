#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import SOAR
NULL

#' Autentificación en ixplorer
#'
#' Hace la conexión a su repositorio a través del gadget de ixplorer y
#' permite crear y revisar tiquetes sin tener que re-ingresar sus credenciales
#'
#' @export
add_token <- function() {

  ui <- miniPage(
    gadgetTitleBar("Autentificación en ixplorer",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancelar",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Listo",
                                              primary = TRUE)),
    miniContentPanel(
      textInput(inputId = "ixplorer_url",
                label = "URL de ixplorer",
                width = "100%",
                placeholder = "Copie su ixplorer URl aquí."),
      uiOutput("token_user"),
      checkboxInput(inputId = "token_persist",
                    value = 1,
                    label = "Persistencia de las credenciales para este repositorio. (No usar en computadoras compartidas)",
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
                    label = "Token de acceso",
                    width = "100%",
                    placeholder = "Ingrese su Token de acceso aquí"),
              textInput(inputId = "ixplorer_user_name",
                        label = "Su nombre de usuario.",
                        width = "100%",
                        placeholder = "Ingrese su nombre de usuario aquí."))
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
