# Global ----------------------------------------------------------------------

  get_data <- function() {
    lista_proyectos <- list()
    lista_proyectos$primero <- c("uno", "dos")
    lista_proyectos$segundo <- c("hola", "hello", "goodbye")
    return(lista_proyectos)
  }

  get_projects <- function(ixplorer_data) {
    proyectos <- names(ixplorer_data)
    return(proyectos)
  }

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  lista_proyectos <- get_data()
  proyectos <- get_projects(lista_proyectos)

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
                  tabPanel(repo,
                           h2(paste("encabezado de ", repo))#,
  #                         linkedScatterUI(repo)
                           ))
      })
   }
   })

#  for (proyecto in proyectos) {
#    lapply(lista_proyectos[proyecto][[1]], function(repo) {
#       callModule(linkedScatter, repo, datos = mtcars)
#    })
#  }


}

