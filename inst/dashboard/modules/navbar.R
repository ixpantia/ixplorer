# example app for prepending/appending a navbarMenu
ui <- navbarPage("Navbar page", id = "tabs",
                 tabPanel("Home",
                 h2("home")
              )
)
server <- function(input, output, session) {


  lista_proyectos <- list()
  #  lista_proyectos$primero <- c("dos")
  lista_proyectos$primero <- c("uno", "dos")
  lista_proyectos$segundo <- c("hola", "hello", "goodbye")

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

  observe({
    lapply(proyectos, function(proyecto) {
    appendTab(inputId = "tabs",
      navbarMenu(proyecto,
          uiOutput("repositories")
      #  lapply(lista_proyectos[proyecto][[1]], function(repo) {
      #             appendTab(repo,  paste("repo page from proyecto"))
      #        })
        #tabPanel("Drop1", paste("Drop1 page from", proyecto, lista_proyectos[proyecto][[1]])),
        #tabPanel("Drop2", paste("Drop2 page from", proyecto)),
        #tabPanel("Drop3", paste("Drop3 page from", proyecto))
        ))
    })
  })
}

shinyApp(ui, server)
