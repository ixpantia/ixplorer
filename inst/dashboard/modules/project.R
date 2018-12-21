project_body_UI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("tab-item"))

}

project_body <- function(input, output, session,
                         lista_proyectos = "No hay proyectos") {

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
}

