# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  # Data ----------------------------------------------------------------------
  working_directory <- getwd()
  print(working_directory)
  lista_proyectos <- get_data()
  proyectos <- get_projects(lista_proyectos)

  # Projects ------------------------------------------------------------------

  for (proyecto in proyectos) {
    lapply(proyectos, function(proyecto) {
      callModule(project, proyecto, project_name = proyecto)
    })
  }

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
            h2(paste("encabezado de ", repo)),
            repository_UI(repo)))
      })
    }
    for (proyecto in proyectos) {
        prependTab(
          inputId = proyecto,
          tabPanel("Overview",
            h2(paste("pagina principal de ", proyecto)),
            project_UI(proyecto)))
    }
  })
}
