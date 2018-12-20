# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  lista_proyectos <- list()
  lista_proyectos$primero <- c("uno", "dos")
  lista_proyectos$segundo <- c("hola", "hello", "goodbye")
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

  proyectos <- names(lista_proyectos)
  repositorios <- c()

  for (proyecto in proyectos) {
    repositorio <- unname(unlist(lista_proyectos[proyecto]))
    repositorios <- c(repositorios, repositorio)
  }

  bodies <- c(proyecto, repositorios)
  bodies <- make.names(bodies)

 lapply(bodies, function(body) {
   callModule(project_body, body,
              body = body)
 })

 output$new_body <- renderUI({
   tabItems(
    lapply(bodies, function(body) {
       tabItem(
        project_body_UI(body)
      )
    })
   )
 })
}
