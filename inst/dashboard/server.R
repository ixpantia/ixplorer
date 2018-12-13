# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  callModule(sidebar_elements, "ix_interface_1",
             proyecto = "primero",
             repositorios = c("uno", "dos"))
  callModule(sidebar_elements, "ix_interface_2",
             proyecto = "segundo",
             repositorios = c("hola", "hello", "goodbye"))

  output$new_sidebar <- renderUI({

    lista_proyectos <- c("ix_interface_1", "ix_interface_2")

    lapply(lista_proyectos, function(i) {
      sidebarMenu(
        sidebar_elements_UI(i)
      )
    })
  })

}
