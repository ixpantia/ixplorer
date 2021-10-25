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
    gadgetTitleBar("ixplorer authentication",
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
      # CHANGE!!! input$ixplorer_url
      instance <- sub("\\..*", "", input$ixplorer_url)

      # Code for using keyring (To be implemented later)
      # verify_cred <- tryCatch(
      #   keyring::key_get(paste0("token_", instance)),
      #   error = function(cond) "no_credentials")

      if (Sys.getenv("IXURL") == "") {
        verify_cred <- "no_credentials"
      } else {
        verify_cred <- Sys.getenv("IXURL")
      }

      if (verify_cred == "no_credentials") {
          div(textInput(inputId = "ixplorer_token",
                    label = "Access Token",
                    width = "100%",
                    placeholder = "Enter your Access Token here"),
              textInput(inputId = "ixplorer_user_name",
                        label = "Your username.",
                        width = "100%",
                        placeholder = "Enter your username here."),
              textInput(inputId = "ixplorer_project",
                        label = "Your project.",
                        width = "100%",
                        placeholder = "Enter your project here.")
              )
      }

    })

    observeEvent(input$done, {

      Sys.setenv("IXURL"   = input$ixplorer_url)
      Sys.setenv("IXTOKEN" = input$ixplorer_token)
      Sys.setenv("IXUSER"  = input$ixplorer_user_name)
      Sys.setenv("IXPROJECT" = input$ixplorer_project)
      # Sys.setenv("IXREPO"  = input$ixplorer_repo_name)


      url     <- paste0("IXURL=", input$ixplorer_url)
      token   <- paste0("IXTOKEN=", input$ixplorer_token)
      user    <- paste0("IXUSER=", input$ixplorer_user_name)
      project <- paste0("IXPROJECT=", input$ixplorer_project)
      # repo    <- paste0("IXREPO=", input$ixplorer_repo_name)


      access_data <- c(url, token, user, project)

      if (input$token_persist == 1) {
        working_directory <- rstudioapi::getActiveProject()
        ixplorer_file <- paste0(working_directory, "/.ixplorer")
        conn <- file(ixplorer_file, open = "w")
        writeLines(access_data, con = conn, sep = "\n", useBytes = FALSE)
        close(conn)

        gitignore <- paste0(working_directory, "/.gitignore")
        if (file.exists(gitignore)) {
          conn <- file(gitignore)
          archivos_ignorados <- readLines(conn)
          writeLines(c(archivos_ignorados,".ixplorer"), conn) #lo sobre escribe
          close(conn)
        } else {
          conn <- file(gitignore, open = "w")
          writeLines(".ixplorer", con = conn, sep = "\n", useBytes = FALSE)
          close(conn)
        }

      }
      stopApp(NULL)
    })


      # Code for using keyring (To be implemented later)
      # # CHANGE !!! input$ixplorer_url
      # instance <- sub("\\..*", "", input$ixplorer_url)
      #
      # verify_cred <- tryCatch(
      #   keyring::key_get(paste0("token_", instance)),
      #   error = function(cond) "no_credentials")
      #
      #
      # if (verify_cred == "no_credentials") {
      #   # CHANGE !!! input$
      #   if (is.null(input$ixplorer_url) == FALSE |
      #       is.null(input$ixplorer_token) == FALSE |
      #       is.null(input$ixplorer_user_name) == FALSE |
      #       is.null(input$ixplorer_project)) {
      #
      #     keyring::key_set_with_value(
      #       service = paste0("token_", instance),
      #       # CHANGE !!! input$
      #       password = paste(input$ixplorer_url,
      #                        input$ixplorer_token,
      #                        input$ixplorer_user_name,
      #                        input$ixplorer_project,
      #                        input$token_persist,
      #                        sep = "/"))
      #
      #     # Accion temporal para guardar url ingresado
      #     # esto deberia de pasar al keyring, pero en funcion de current_tickets
      #     # necesitamos un pedazo del url para poder abrir el archivo de keyrin
      #     # (huevo gallina situacion) asi que vamos a escribir TEMPORALMENTE
      #     # a un archivo para probar funcionalidad
      #
      #     credentials <- tibble::tribble(
      #       ~url, ~token, ~user_name, ~project, ~t
      #     )
      #
      #     url <- as.data.frame(input$ixplorer_url)
      #     readr::write_csv(url, paste0(here::here(), "archivo_temp.csv"))
      #
      #   }
      #
      # } else {
      #
      #   }
      #
      #   stopApp(TRUE)
      # })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(TRUE)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
