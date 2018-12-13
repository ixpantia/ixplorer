# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  lista_proyectos <- list()
  lista_proyectos$primero <- c("uno", "dos")
  lista_proyectos$segundo <- c("hola", "hello", "goodbye")


   lapply(names(lista_proyectos), function(proyecto) {
     callModule(sidebar_elements, proyecto,
                proyecto = proyecto,
               repositorios = unname(unlist(lista_proyectos[proyecto])))
   })

  output$new_sidebar <- renderUI({

    proyectos <- names(lista_proyectos)

    lapply(proyectos, function(proyecto) {
      sidebarMenu(
        sidebar_elements_UI(proyecto)
      )
    })
  })

}
