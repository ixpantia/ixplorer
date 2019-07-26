#' @import shiny
#' @import miniUI
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

      textInput(inputId = "ixplorer_token",
                label = "Token de acceso",
                width = "100%",
                placeholder = "Ingrese su Token de acceso aquí"),

      textInput(inputId = "ixplorer_url",
                label = "URL de ixplorer",
                width = "100%",
                placeholder = "Copie su ixplorer URl aquí."),

      textInput(inputId = "ixplorer_project_name",
                label = "Nombre proyecto",
                width = "100%",
                placeholder = "Ingrese el nombre del proyecto aquí"),

      textInput(inputId = "ixplorer_repo_name",
                label = "Nombre del repositorio ixplorer.",
                width = "100%",
                placeholder = "Copie el nombre del repositorio ixplorer aquí."),

      textInput(inputId = "ixplorer_user_name",
                label = "Su nombre de usuario.",
                width = "100%",
                placeholder = "Ingrese su nombre de usuario aquí."),

      checkboxInput(inputId = "token_persist",
                    value = 0,
                    label = "Persistencia de las credenciales. (No usar en computadoras compartidas)",
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

      token   <- paste0("IXTOKEN=", input$ixplorer_token)
      url     <- paste0("IXURL=", input$ixplorer_url)
      project <- paste0("IXPROJECT=", input$ixplorer_project_name)
      repo    <- paste0("IXREPO=", input$ixplorer_repo_name)
      user    <- paste0("IXUSER=", input$ixplorer_user_name)

      access_data <- c(token, url, project, repo, user)

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

    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
