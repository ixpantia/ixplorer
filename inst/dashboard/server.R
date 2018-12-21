# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  lista_proyectos <- list()
#  lista_proyectos$primero <- c("dos")
  lista_proyectos$primero <- c("uno", "dos")
  lista_proyectos$segundo <- c("hola", "hello", "goodbye")

  proyectos <- names(lista_proyectos)

  lapply(proyectos, function(proyecto) {
   callModule(sidebar_elements, proyecto,
              proyecto = proyecto,
              repositorios = unname(unlist(lista_proyectos[proyecto])))
  })


  output$new_sidebar <- renderMenu({
    lapply(proyectos, function(proyecto) {
      sidebarMenu(
        sidebar_elements_UI(proyecto)
      )
    })
  })

 #callModule(project_body, "cuerpos",
 #           lista_proyectos = lista_proyectos)

 output$new_body <- renderUI({
 #  tabItems(
 #         project_body_UI("cuerpos")
 #  )
 })

}
