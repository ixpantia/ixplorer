#' @import shiny
#' @import miniUI
#' @import shiny.i18n
#' @import shinyWidgets
NULL

#' @title Create ticket
#' @description Create tickets (title and body) from the ixplorer addin without
#'  losing ideas during your workflow.
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#'
#' @export
create_tickets <- function(instance = "saved") {

  # Look for instance ---------------------------------------------------------

  # The default instance value is "saved", so it first looks for a saved
  # keyring in case user forgets to authenticate. It then chooses the last saved
  # keyring.
  # If the user chooses a specific instance other than "saved" ,
  # such as "secure" or "prueba" then that instance is used

  if (instance == "saved") {

    # It looks in session
    if (Sys.getenv("ixplorer_instance") != "") {

      instance <- Sys.getenv("ixplorer_instance")
      message("Current instance is ", instance)
      no_instance = FALSE


      # If there is no enviroment variable it means user is looking for
      # a previously saved instance
    } else if (Sys.getenv("ixplorer_instance") == "") {

      saved_instances <- keyring::keyring_list() %>%
        filter(stringr::str_detect(keyring, "ixplorer_"))

      # if there are saved instances, then it chooses the instance that was last saved
      if (nrow(saved_instances) > 0) {

        last_saved <- saved_instances[1,1]
        instance <- last_saved
        message("Current instance is ", instance)
        no_instance = FALSE


        # When there are no saved instances, then a message is printed
      } else {
        message("There are no saved instances")
        no_instance = TRUE
      }

    }

    # If the user chooses an instance other than "saved" then it looks for
    # the specified instance in previously saved keyrings

  } else {

    saved_instances <- keyring::keyring_list() %>%
      select(keyring) %>%
      filter(keyring == paste0("ixplorer_",instance))

    if (nrow(saved_instances) > 0) {
      instance <- toString(saved_instances[1])
      message("Current instance is ", instance)
      no_instance = FALSE

    } else {
      message("No credentials for ", instance)
      no_instance = FALSE
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

  # UI ------------------------------------------------------------------------

  ui <- miniPage(
    miniTitleBar(i18n$t("Create new ticket"),
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = i18n$t("Cancel"),
                                                   primary = TRUE)
                   ),

    miniContentPanel(
      verbatimTextOutput(i18n$t("warning"), placeholder = FALSE),

      textInput(inputId = "ticket_title",
                label = i18n$t("Ticket title"),
                width = "150%",
                placeholder = i18n$t("Brief description of your ticket")),

      textAreaInput(inputId = "ticket_description",
                    label = i18n$t("Description"),
                    width = "190%",
                    height = "100%",
                    resize = "vertical",
                    rows = 13,
                    placeholder = i18n$t("Describe the ticket you have found"))
    ),
    miniButtonBlock(
      actionButton(inputId = "create", label = i18n$t("Create ticket"),
                   style = "color: #fff; background-color: #73CF56")
    )
  )

  # Server --------------------------------------------------------------------

  server <- function(input, output, session) {


    # output$warning <- renderText({
    #   msg <- ifelse (credentials == "no_credentials",
    #     "No credential file available", "")
    #   return(msg)
    # })

    # Botones ----------------------------------------------------------------
    observeEvent(input$cancel, {
      # do nothing
      stopApp()
    })

    # observeEvent(input$create, {
    #      check <-  tryCatch(gitear::create_issue(
    #        base_url = credentials$url,
    #        api_key = credentials$token,
    #        owner = credentials$owner,
    #        repo = repository,
    #        title = input$ticket_title,
    #        body =  input$ticket_description),
    #        error = function(cond)
    #          "Invalido")
    observeEvent(input$create, {
      check <-  tryCatch(gitear::create_issue(
        base_url = keyring::key_get("ixplorer_url", keyring = instance),
        api_key = keyring::key_get("ixplorer_token", keyring = instance),
        owner = keyring::key_get("ixplorer_project", keyring = instance),
        repo = keyring::key_get("ixplorer_repo", keyring = instance),
        title = input$ticket_title,
        body =  input$ticket_description),
        error = function(cond)
          "Invalido")

      if (is.list(check)) {
           print(check)
           message("Your ticket has been generated successfully")
         } else {
           if (check != "Invalido") {
             print("No ticket created due to invalid credentials. Please use the authentication gadget")
           }
         }

      stopApp()
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
#' @title Crear tiquete
#' @description Cree tiquetes (título y cuerpo) desde el add-in de ixplorer sin
#' perder las ideas durante su flujo de trabajo
#'
#' @param instancia instancia de ixplorer (Ejemplo: "secure", "masterclass", "prueba")
#'
#' @export
crear_tiquetes <- function(instancia = "guardada") {

  if (instancia == "guardada") {

    create_tickets(instance = "saved")

  } else {

    create_tickets(instance = instancia)

  }
}
