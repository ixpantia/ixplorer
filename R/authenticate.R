#' @import shiny
#' @import miniUI
#' @import shiny.i18n
#' @import shinyWidgets
NULL

#' @title Authentication in ixplorer
#' @description Saves to your computer
#' through the authentication system of your computer's OS.
#'
#' @details Currently needs to be done everytime a new session is started.
#' In case the credentials already exist,
#' just enter the url and confirm if you want to keep the credentials
#' on your computer or want to delete them after the next query.
#'
#' @export
add_token <- function() {

  # Look for instance ---------------------------------------------------------

  if (Sys.getenv("ixplorer_instance") != "") {

    instance <- Sys.getenv("ixplorer_instance")
    no_instance = FALSE
    message("Current instance is ", instance)


    # If there is no environment variable it means user is looking for
    # a previously saved instance
  } else if (Sys.getenv("ixplorer_instance") == "") {

    saved_instances <- keyring::keyring_list() %>%
      filter(stringr::str_detect(keyring, "ixplorer_"))

    # if there are saved instances, then it chooses the instance that was last saved
    if (nrow(saved_instances) > 0) {

      last_saved <- saved_instances[1,1]
      instance <- last_saved
      no_instance = FALSE
      message("Current instance is ", instance)



    } else {
      no_instance = TRUE
    }
  }


  # Define translator ---------------------------------------------------------

  i18n <- shiny.i18n::Translator$new(
    translation_json_path = "https://storage.googleapis.com/ixplorer/translation.json"
    )

  # Set translation language --------------------------------------------------

  if (no_instance == TRUE) {
    i18n$set_translation_language("en")
  } else {

    language <- keyring::key_get("ixplorer_language", keyring = instance)
    i18n$set_translation_language(language)

    }

  # Saved instances -----------------------------------------------------------

  saved_instances <- keyring::keyring_list() %>%
    filter(stringr::str_detect(keyring, "ixplorer_"))

  saved_instances <- stringr::str_remove(saved_instances$keyring, "ixplorer_")


  # UI ------------------------------------------------------------------------

  ui <- miniPage(
    gadgetTitleBar(i18n$t("ixplorer authentication"),
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = i18n$t("Cancel"),
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = i18n$t("Done"),
                                              primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel(i18n$t("Sign in"), icon = icon("pencil"),
                   miniContentPanel(
                     textInput(inputId = "ixplorer_url",
                               label = "ixplorer URL",
                               width = "100%",
                               placeholder = i18n$t(
    "Copy your ixplorer URL here. E.g. https://prueba.ixpantia.com")),
                     uiOutput("token_user"),
                     textOutput("text")
                     # checkboxInput(inputId = "token_persist",
                     #               value = 1,
                     #               label = "Persistence of credentials on this computer.
                     #               (Do not use on shared computers)",
                     #               width = "100%"
                     # )
                   )
      ),
      miniTabPanel(i18n$t("ixploring"), icon = icon("book"),
                   miniContentPanel(
                     shinyWidgets::checkboxGroupButtons(
                       inputId = "buttons",
                       label = i18n$t("Your saved instances"),
                       choices = saved_instances
                     ),
                     uiOutput("projects"),
                     uiOutput("repos")
                   )
      )
    )
    )

  # server --------------------------------------------------------------------
  server <- function(input, output, session) {


    # Defining an instance from the link
    instance <- reactive({

      instance <- sub("\\..*", "", input$ixplorer_url) %>%
        stringr::str_split("//")

      instance <- paste0("ixplorer_",instance[[1]][2])

      return(instance)

    })



    # Getting a vector of the keyrings saved in the computer

    keyrings <- reactive({

      keyrings <- keyring::keyring_list()$keyring

      return(keyrings)

    })


# When the instance in the link is not in the keyring vector then we ask for
# the complete credentials

    output$token_user <- renderUI({


      req(input$ixplorer_url) # Here we require that the user pastes a link


      if (!(instance() %in% keyrings())) {
        div(textInput(inputId = "ixplorer_token",
                      label = i18n$t("Access Token"),
                      width = "100%",
                      placeholder = i18n$t("Enter your Access Token here")),
            textInput(inputId = "ixplorer_user_name",
                      label = i18n$t("Your username."),
                      width = "100%",
                      placeholder = i18n$t("Enter your username here.")),
            textInput(inputId = "ixplorer_project",
                      label = i18n$t("Your project."),
                      width = "100%",
                      placeholder = i18n$t("Enter your project here.")),
            textInput(inputId = "ixplorer_repo",
                      label = i18n$t("Your repository."),
                      width = "100%",
                      placeholder = i18n$t("Enter your repository name here.")),
            selectInput(inputId = "ixplorer_language",
                        label = i18n$t("Choose a language"),
                        choices = i18n$get_languages())
        )
      }
    })


# If the instance is found inside the keyring vector, we direct the user
# to the second tab

    output$text <- renderText({

      req(instance() %in% keyrings())

i18n$t("Seems there is already an instance for this url. Use the ixploring tab")
    })





# In the second tab we get the projects from the saved instance
    output$projects <- renderUI({


      req(input$buttons) # Here we require that the instance can
      # be found in the keyring vector to display
      # the UI

      button_instance <- paste0("ixplorer_", input$buttons)

      org_list <- gitear::get_organizations(  # Here we get the project list

        base_url = keyring::key_get("ixplorer_url" , keyring = button_instance),
        api_key = keyring::key_get("ixplorer_token", keyring = button_instance)

      )

      div(
        shinyWidgets::pickerInput(
        inputId = "projects",
        label = i18n$t("Projects"),
        choices = org_list$username,
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(title = "Projects",
                                `live-search` = TRUE,
                                actionsBox = FALSE,
                                deselectAllText = "Clear Search"))
      )

    })

# Once the projects selection ins done, we ask for a repo selection
    output$repos <- renderUI({

      req(input$projects) # Here we require a selection on the projects picker

      button_instance <- paste0("ixplorer_", input$buttons)

      list_repos<-gitear::get_list_repos_org(
        base_url = keyring::key_get("ixplorer_url" , keyring = button_instance),
        api_key = keyring::key_get("ixplorer_token", keyring = button_instance),
        org = input$projects)

      div(
        shinyWidgets::pickerInput(
        inputId = "repos",
        label = i18n$t("Repositories"),
        choices = list_repos$name,
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(title = "Projects",
                                `live-search` = TRUE,
                                actionsBox = FALSE,
                                deselectAllText = "Clear Search", size = 4)))

    })


    observeEvent(input$done, {

      if (is.null(input$projects) == TRUE |
          is.null(input$repos) == TRUE) {

        keyring::keyring_create(instance())

        keyring::key_set_with_value(
          "ixplorer_url", password = input$ixplorer_url,
          keyring = instance()
        )

        keyring::key_set_with_value(
          "ixplorer_token", password = input$ixplorer_token,
          keyring = instance()
        )

        keyring::key_set_with_value(
          "ixplorer_user_name", password = input$ixplorer_user_name,
          keyring = instance()
        )

        keyring::key_set_with_value(
          "ixplorer_project", password = input$ixplorer_project,
          keyring = instance()
        )

        keyring::key_set_with_value(
          "ixplorer_repo", password = input$ixplorer_repo,
          keyring = instance()
        )

        keyring::key_set_with_value(
          "ixplorer_link", password = input$ixplorer_url,  #save the url as well
          keyring = instance()
        )

        keyring::key_set_with_value(
          "ixplorer_language", password = input$ixplorer_language,
          keyring = instance()
        )



        Sys.setenv(ixplorer_instance=instance()) # variable to check miniUI workflow

      } else {

        button_instance <- paste0("ixplorer_", input$buttons)

        keyring::key_set_with_value(
          "ixplorer_project", password = input$projects,
          keyring = button_instance
        )



        keyring::key_set_with_value(
          "ixplorer_repo", password = input$repos,
          keyring = button_instance
        )

        Sys.setenv(ixplorer_instance=button_instance)

      }


      # observeEvent(input$ixplorer_language, {
      #   # This print is just for demonstration
      #   print(paste("Language change!", input$ixplorer_language))
      #   # Here is where we update language in session
      #   shiny.i18n::update_lang(session, input$ixplorer_language)
      # })


      ## Parte del ixplorer file
      # access_data <- c(url, token, user, project)
      #
      # if (input$token_persist == 1) {
      #   # working_directory <- rstudioapi::getActiveProject()
      #   ixplorer_file <- paste0(working_directory, "/.ixplorer")
      #   conn <- file(ixplorer_file, open = "w")
      #   writeLines(access_data, con = conn, sep = "\n", useBytes = FALSE)
      #   close(conn)
      #
      #   gitignore <- paste0(working_directory, "/.gitignore")
      #   if (file.exists(gitignore)) {
      #     conn <- file(gitignore)
      #     archivos_ignorados <- readLines(conn)
      #     writeLines(c(archivos_ignorados,".ixplorer"), conn) #lo sobre escribe
      #     close(conn)
      #   } else {
      #     conn <- file(gitignore, open = "w")
      #     writeLines(".ixplorer", con = conn, sep = "\n", useBytes = FALSE)
      #     close(conn)
      #   }
      #
      # }
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

