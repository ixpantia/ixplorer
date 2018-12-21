sidebar_elements_UI <- function(id) {

  ns <- NS(id)

    uiOutput(ns("proyecto"))
}


sidebar_elements <- function(input, output, session,
                             proyecto = "No hay proyectos",
                             repositorios = c("No hay repositorios")) {

  output$proyecto <- renderUI({
    appendTab(inputId = "tabs",
              tabPanel(make.names(proyecto),
                       tabsetPanel(
                         #lapply(lista_proyectos[proyecto][[1]], function(repo) {
                         lapply(c("1", "2"), function(repo) {
                           appendTab(inputId = proyecto,
                                     tabPanel(make.names(repo)))
                         })
                 )))
       })
}
