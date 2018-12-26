repository_UI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, plotlyOutput(ns("plot1"))),
    column(6, plotOutput(ns("plot2")))
  )
}

repository <- function(input, output, session,
                         repo_data = "No Repositories") {

  # Incidentes abiertos
  open_issues <- gitear::get_issues_open_state(
    base_url = "https://gitear.ixpantia.com/",
    api_key = "47c0be813944aaa0132d77a8110d48e9d3a644af",
    owner = "ixplorer",
    repo = "sitio_pruebas")

  open_issues <- jsonlite::flatten(open_issues)
  etiquetas_abiertas <- open_issues$labels
  etiquetas_abiertas <- do.call(rbind.data.frame, etiquetas_abiertas)
  open_issues <- data.frame(etiquetas_abiertas,  open_issues)
  open_issues_labels <- open_issues %>%
    select(name, state, created_at, updated_at)

  # Incidentes cerrados
  closed_issues <- gitear::get_issues_closed_state(
    base_url = "https://gitear.ixpantia.com/",
    api_key = "47c0be813944aaa0132d77a8110d48e9d3a644af",
    owner = "ixplorer",
    repo = "sitio_pruebas")

  closed_issues <- jsonlite::flatten(closed_issues)
  etiquetas_cerradas <- closed_issues$labels
  etiquetas_cerradas <- do.call(rbind.data.frame, etiquetas_cerradas)
  closed_issues <- data.frame(etiquetas_cerradas,  closed_issues)
  closed_issues_labels <- closed_issues %>%
    select(name, state, created_at, updated_at)

  # Union de incidentes cerrados/abiertos con etiquetas
  # y formato de fechas:
  int = interval(today() - 7, today() + 1) #Esto porque no agarra el ultimo
  incidentes <- rbind(closed_issues_labels, open_issues_labels) %>%
    mutate(created_at = lubridate::ymd_hms(created_at)) %>%
    mutate(updated_at = lubridate::ymd_hms(updated_at))

  incidentes <- incidentes %>%
    mutate(state = ifelse(created_at %within% int, "last", incidentes$state))

  output$plot1 <- renderPlotly({
    p1 <- plot_ly(incidentes, y = ~ name, color = ~ state) %>%
      add_histogram() %>%
      layout(barmode = "stack")
    return(p1)
  })

  output$plot2 <- renderPlot({
   plot(repo_data)
  })

}

