project_body_UI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("tab-item"))

}

project_body <- function(input, output, session,
                         body = "empty.menu") {

#  proyectos <- names(lista_proyectos)
#  repositorios <- c()
#
#  for (proyecto in proyectos) {
#    repositorio <- unname(unlist(lista_proyectos[proyecto]))
#    repositorios <- c(repositorios, repositorio)
#  }
#
#  bodies <- c(proyecto, repositorios)
#  bodies <- make.names(bodies)
#
  output$tab-item <- renderUI({
      lapply(bodies, function(body) {
          tabItem(tabName = body,
            h2(paste("hello, ", body))
          )
      })
  })
}

