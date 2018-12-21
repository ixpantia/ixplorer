library(shiny)
# example app for prepending/appending a navbarMenu
ui <- navbarPage("Navbar page", id = "tabs")

server <- function(input, output, session) {


  lista_proyectos <- list()
  #  lista_proyectos$primero <- c("dos")
  lista_proyectos$lagarita <- c("Diego", "Bellota")
  lista_proyectos$sancarlos <- c("hola", "hello", "goodbye")

  proyectos <- names(lista_proyectos)

  proyectos <- names(lista_proyectos)
  repositorios <- c()

  for (proyecto in proyectos) {
    repositorio <- unname(unlist(lista_proyectos[proyecto]))
    repositorios <- c(repositorios, repositorio)
  }

  bodies <- c(proyectos, repositorios)
  bodies <- make.names(bodies)

  output$repositories <- renderUI(
        for (repo in lista_proyectos[proyecto][[1]]) {
          tabPanel(repo, paste("Drop1 page from"))
        }
  )

  make_panel <- function(prev_input, panel_name) {
    appendTab(inputId = proyecto,
              tabPanel("repo"))
  }

  observe({
    lapply(proyectos, function(proyecto) {
      appendTab(inputId = "tabs",
        tabPanel(proyecto,
          tabsetPanel(id = proyecto)
         ))
      })
   })

  observe({
    for (proyecto in proyectos) {
      lapply(lista_proyectos[proyecto][[1]], function(repo) {
        appendTab(inputId = proyecto,
                  tabPanel(repo))
      })
   }
   })

}
shinyApp(ui, server)
