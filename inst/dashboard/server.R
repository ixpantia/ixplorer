
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
        appendTab(
          inputId = proyecto,
          tabPanel(repo,
            h2(paste("encabezado de ", repo)),
            project_UI(repo)))
      })
    }
  })

  for (proyecto in proyectos) {
    lapply(lista_proyectos[proyecto][[1]], function(repo) {
      callModule(project, repo, project_data = mtcars)
    })
  }

}

