#' @import shiny
#' @import miniUI
NULL

#' @title Create ticket
#' @description Create tickets (title and body) from the ixplorer addin without
#'  losing ideas during your workflow.
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in ixplorer
#' @param repository the name of the repository where the tickets are
#'
#' @export
create_tickets <- function(instance = "saved") {

  # Code for using keyring (To be implemented later)---------------------------
  # credentials <- tryCatch(
  #   keyring::key_get(paste0("token_", instance)),
  #   error = function(cond) "no_credentials")


  # if (credentials != "no_credentials") {
  #   credentials <- credentials %>%
  #     stringr::str_split("/", simplify = TRUE) %>%
  #     tibble::as_tibble() %>%
  #     magrittr::set_names(c("url", "token",
  #                           "user", "persistence")) %>%
  #     dplyr::mutate(persistence = as.logical(persistence))
  #
  # }

  # Get repository from Rstudio Api -------------------------------------------

  # if (repository == "current") {
  #   repository <- basename(rstudioapi::getActiveProject())
  # }




  # Warning to be checked in ref42 --------------------------------------------
  # output$warning <- renderText({
  #   msg <- if (access_file$empty == TRUE) {
  #     "No hay archivo de credenciales disponible"
  #   } else {
  #     set_authentication(access_data = access_file$gitear_access)
  #   }
  #   return(msg)
  # })

  # Read credentials from .ixplorer TEMPORAL-----------------------------------
  # access_file <- ixplorer:::verify_ixplorer_file()
  # ixplorer_url <- Sys.getenv("IXURL")
  #
  # credentials <- tibble::tribble(
  #   ~url, ~token, ~user, ~owner,
  #   Sys.getenv("IXURL"), Sys.getenv("IXTOKEN"), Sys.getenv("IXUSER"), Sys.getenv("IXPROJECT")
  # )
  #
  # instance <- sub("\\..*", "", ixplorer_url)

  # Code for using keyring (To be implemented later)---------------------------
  # if(credentials$persistence == FALSE) {
  #   keyring::key_delete(paste0("token_", instance))
  # }

  # Look for instance ---------------------------------------------------------

  # The default instance value is "saved", so it first looks for a saved
  # keyring in case user forgets to authenticate. It then chooses the last saved
  # keyring.
  # If the user chooses a specific instance other than "saved" ,
  # such as "secure" or "prueba" then that instance is used

  if (instance == "saved"){

    # It looks in session
    if (Sys.getenv("ixplorer_instance") != "") {

      instance <- Sys.getenv("ixplorer_instance")


      # If there is no enviroment variable it means user is looking for
      # a previously saved instance
    } else if (Sys.getenv("ixplorer_instance") == ""){

      saved_instances <- keyring::keyring_list() %>%
        filter(stringr::str_detect(keyring, "ixplorer_"))

      # if there are saved instances, then it chooses the instance that was last saved
      if (nrow(saved_instances) > 0) {

        last_saved <- saved_instances[1,1]
        instance <- last_saved


        # When there are no saved instances, then a message is printed
      } else {
        message("There are no saved instances")
      }

    }

    # If the user chooses an instance other than "saved" then it looks for
    # the specified instance in previously saved keyrings

  } else {

    saved_instances <- keyring::keyring_list() %>%
      select(keyring) %>%
      filter(keyring == paste0("ixplorer_",instance))

    if (nrow(saved_instances) > 0){
      instance <- toString(saved_instances[1])

    } else {
      message("No credentials for ", instance)
    }



  }

  # UI ------------------------------------------------------------------------

  ui <- miniPage(
    miniTitleBar("Create new ticket",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = TRUE)
                   ),

    miniContentPanel(
      verbatimTextOutput("warning", placeholder = FALSE),

      textInput(inputId = "ticket_title",
                label = "Ticket title",
                width = "150%",
                placeholder = "Brief description of your ticket"),

      textAreaInput(inputId = "ticket_description",
                    label = "Description",
                    width = "190%",
                    height = "100%",
                    resize = "vertical",
                    rows = 13,
                    placeholder = "Describe the ticket you have found")
    ),
    miniButtonBlock(
      actionButton(inputId = "create", label = "Create ticket",
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
#' @param propietario el nombre del proyecto donde el repositorio está ubicado en ixplorer
#' @param repositorio nombre del repositorio donde están los tiquetes
#'
#' @export
crear_tiquetes<- function(instancia = "guardada") {

  if(instancia == "guardada"){

    create_tickets(instance = "saved")

  } else {

    create_tickets(instance = instancia)

  }
}
