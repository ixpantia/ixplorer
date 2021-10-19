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
create_tickets <- function(instance, owner, repository = "current") {

  credentials <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credentials")


  if(credentials != "no_credentials") {
    credentials <- credentials %>%
      stringr::str_split("/", simplify = TRUE) %>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("url", "token",
                            "user", "persistence")) %>%
      dplyr::mutate(persistence = as.logical(persistence))

  }

  if(credentials$persistence == FALSE) {
    keyring::key_delete(paste0("token_", instance))
  }

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

  server <- function(input, output, session) {


    output$warning <- renderText({
      msg <- ifelse (credentials == "no_credentials",
        "No credential file available", "")
      return(msg)
    })

    # Botones ----------------------------------------------------------------
    observeEvent(input$cancel, {
      # do nothing
      stopApp()
    })

    observeEvent(input$create, {
         check <-  tryCatch(gitear::create_issue(
           base_url = credentials$url,
           api_key = credentials$token,
           owner = owner,
           repo = repository,
           title = input$ticket_title,
           body =  input$ticket_description),
           error = function(cond)
             "Invalido")

         if (is.list(check)){
           print(check)
           message("Your ticket has been generated successfully")
         } else {
           if(check != "Invalido") {
             print("No ticket created due to invalid credentials. Please use the authentication gadget")
           }
         }

      stopApp()
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
