# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  # Data ----------------------------------------------------------------------
  lista_proyectos <- get_data()
  proyectos <- get_projects(lista_proyectos)

  # Projects ------------------------------------------------------------------

    lapply(proyectos, function(proyecto) {
      callModule(project, proyecto, project_name = proyecto)
    })

  observe({
    lapply(proyectos, function(proyecto) {
      appendTab(inputId = "tabs",
        tabPanel(proyecto,
          tabsetPanel(id = proyecto)
         ))
      })
   })

  # Repositories --------------------------------------------------------------
  for (proyecto in proyectos) {
    lapply(lista_proyectos[proyecto][[1]], function(repo) {
      callModule(repository, repo, repo_name = repo, project_name = proyecto)
    })
  }

  observe({
    for (proyecto in proyectos) {
      lapply(lista_proyectos[proyecto][[1]], function(repo) {
        appendTab(
          inputId = proyecto,
          tabPanel(repo,
            h2(paste("Resumen de actividad del repositorio: ", repo)),
            repository_UI(repo)))
      })
    }
    for (proyecto in proyectos) {
        prependTab(
          inputId = proyecto,
          tabPanel("Overview",
            h2(paste("Resumen general del proyecto:", proyecto)),
            project_UI(proyecto)))
    }
  })
}
