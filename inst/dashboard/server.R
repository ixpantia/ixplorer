
# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  # Data ----------------------------------------------------------------------

  lista_proyectos <- get_data()
  proyectos <- get_projects(lista_proyectos)

  # Projects ------------------------------------------------------------------

  for (proyecto in proyectos) {
    lapply(proyectos, function(proyecto) {
      callModule(project, proyecto, project_data = mtcars[, 1:2])
    })
  }

  observe({
    lapply(proyectos, function(proyecto) {
      appendTab(inputId = "tabs",
        tabPanel(proyecto,
          tabsetPanel(id = proyecto),
          appendTab(inputId = proyecto,
                    tabPanel("Project Overview",
                   project_UI(proyecto)))
         ))
      })
   })

  # Repositories --------------------------------------------------------------
  for (proyecto in proyectos) {
    lapply(lista_proyectos[proyecto][[1]], function(repo) {
      callModule(repository, repo, repo_data = mtcars[3:5])
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
  })


}

