# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  lista_proyectos <- list()
  lista_proyectos$primero <- c("uno", "dos")
  lista_proyectos$segundo <- c("hola", "hello", "goodbye")

  proyectos <- names(lista_proyectos)

  # ---------------------------------------------------------------------------
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


  # ---------------------------------------------------------------------------
  proyectos <- names(lista_proyectos)
  repositorios <- c()

  for (proyecto in proyectos) {
    repositorio <- unname(unlist(lista_proyectos[proyecto]))
    repositorios <- c(repositorios, repositorio)
  }

  bodies <- c(proyectos, repositorios)
  bodies <- make.names(bodies)

  output$project_tabitem <- renderUI({
    lapply(bodies, function(body) {
      tabItem(tabName = body,
              h2(paste("hello, ", body))
      )
    })
  })

  output$test <- renderUI({

    bodies <- c("uno", "hello")

    lapply(bodies, function(body) {
        div(tabItem(tabName = body,
                h2("esto es uno")))
    })
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
