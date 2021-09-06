#' @import shiny
#' @import miniUI
NULL

#' @title Crear tiquete
#' @description Crear tiquetes (título y cuerpo) desde el addin de ixplorer sin
#' perder las ideas durante su flujo de trabajo.
#'
#' @param instance instancia de ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param owner el nombre del proyecto donde se encuentra el repositorio en
#' ixplorer
#' @param repository el nombre del repositorio donde están los tiquetes
#'
#' @export
create_tickets <- function(instance, owner, repository = "current") {

  credenciales <- tryCatch(
    keyring::key_get(paste0("token_", instance)),
    error = function(cond) "no_credenciales")


  if(credenciales != "no_credenciales") {
    credenciales <- credenciales %>%
      stringr::str_split("/", simplify = TRUE) %>%
      tibble::as_tibble() %>%
      magrittr::set_names(c("url", "token",
                            "usuario", "persistencia")) %>%
      dplyr::mutate(persistencia = as.logical(persistencia))

  }

  if(credenciales$persistencia == FALSE) {
    keyring::key_delete(paste0("token_", instance))
  }

  ui <- miniPage(
    miniTitleBar("Crear un nuevo tiquete",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancelar",
                                                   primary = TRUE)
                   ),

    miniContentPanel(
      verbatimTextOutput("warning", placeholder = FALSE),

      textInput(inputId = "ticket_title",
                label = "Título del tiquete",
                width = "150%",
                placeholder = "Breve descripción de su tiquete"),

      textAreaInput(inputId = "ticket_description",
                    label = "Descripción",
                    width = "190%",
                    height = "100%",
                    resize = "vertical",
                    rows = 13,
                    placeholder = "Describa el tiquete que ha encontrado")
    ),
    miniButtonBlock(
      actionButton(inputId = "create", label = "Crear tiquete",
                   style = "color: #fff; background-color: #73CF56")
    )
  )

  server <- function(input, output, session) {


    output$warning <- renderText({
      msg <- ifelse (credenciales == "no_credenciales",
        "No hay archivo de credenciales disponible", "")
      return(msg)
    })

    # Botones ----------------------------------------------------------------
    observeEvent(input$cancel, {
      # do nothing
      stopApp()
    })

    observeEvent(input$create, {
         check <-  tryCatch(gitear::create_issue(
           base_url = credenciales$url,
           api_key = credenciales$token,
           owner = owner,
           repo = repository,
           title = input$ticket_title,
           body =  input$ticket_description),
           error = function(cond)
             "Invalido")

         if (is.list(check)){
           print(check)
           message("Su tiquete se ha generado con éxito")
         } else {
           if(check != "Invalido") {
             print("No se ha creado ningún tiquete debido a credenciales inválidas. Porfavor use el gadget de autentificación.")
           }
         }

      stopApp()
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
