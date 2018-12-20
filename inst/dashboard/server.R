# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  lista_proyectos <- list()
  lista_proyectos$primero <- c("dos")
#  lista_proyectos$primero <- c("uno", "dos")
#  lista_proyectos$segundo <- c("hola", "hello", "goodbye")
    proyectos <- names(lista_proyectos)


   lapply(names(lista_proyectos), function(proyecto) {
     callModule(sidebar_elements, proyecto,
                proyecto = proyecto,
               repositorios = unname(unlist(lista_proyectos[proyecto])))
   })


  output$new_sidebar <- renderUI({

    lapply(proyectos, function(proyecto) {
      sidebarMenu(
        sidebar_elements_UI(proyecto)
      )
    })
  })

lapply(proyectos, function(proyecto) {
  callModule(project_body, "body_test",
              project_name = make.names(proyecto))
})

 output$new_body <- renderUI({
   project_body_UI("body_test")
 })

}
