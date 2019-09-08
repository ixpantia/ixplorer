#' @import shiny
#' @import miniUI
NULL

#' Crear tiquete
#'
#' Crear tiquetes (Título y cuerpo) desde el addin de ixplorer sin perder las
#' ideas durante su flujo de trabajo. Los tiquetes serán creados en el
#' repositorio que corresponde a la información dada en el gadget de
#' autentificación.
#'
#' @export
create_tickets <- function() {

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

    access_file <- ixplorer.es:::verify_ixplorer_file()

    output$warning <- renderText({
      msg <- if (access_file$empty == TRUE) {
        "No hay archivo de credenciales disponible"
      } else {
        set_authentication(access_data = access_file$gitear_access)
      }
      return(msg)
    })

    # Botones ----------------------------------------------------------------
    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })

    observeEvent(input$create, {
         check <-  gitear::create_issue(
           base_url = paste("https://", strsplit(Sys.getenv("IXURL"), "//")[[1]][2]),
           api_key = Sys.getenv("IXTOKEN"),
           owner = Sys.getenv("IXPROJECT"),
           repo = Sys.getenv("IXREPO"),
           title = input$ticket_title,
           body =  input$ticket_description)

         if (check$status_code == 404) {
           print("No se ha creado ningún tiquete debido a credenciales inválidas. Porfavor use el gadget de autentificación.")
         } else {
           check
         }
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
