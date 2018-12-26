project_UI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(6, plotlyOutput(ns("plot1"))),
    column(6, plotOutput(ns("plot2")))
  )

}

project <- function(input, output, session,
                    project_data = "No Projects") {

  # Incidentes abiertos sitio_pruebas
  open_issues_sitio <- gitear::get_issues_open_state(
    base_url = "https://gitear.ixpantia.com/",
    api_key = "47c0be813944aaa0132d77a8110d48e9d3a644af",
    owner = "ixplorer",
    repo = "sitio_pruebas")

  open_issues_sitio <- jsonlite::flatten(open_issues_sitio)

  etiquetas_abiertas <- open_issues_sitio$labels
  etiquetas_abiertas <- do.call(rbind.data.frame, etiquetas_abiertas)
  open_issues_sitio <- data.frame(etiquetas_abiertas,  open_issues_sitio)
  open_issues_labels_sitio <- open_issues_sitio %>%
    select(name, state, created_at, updated_at)

  # Incidentes abiertos asignaciones
  open_issues_asignaciones <- gitear::get_issues_open_state(
    base_url = "https://gitear.ixpantia.com/",
    api_key = "47c0be813944aaa0132d77a8110d48e9d3a644af",
    owner = "ixplorer",
    repo = "asignaciones")

  open_issues_asignaciones <- jsonlite::flatten(open_issues_asignaciones)

  etiquetas_abiertas <- open_issues_asignaciones$labels
  etiquetas_abiertas <- do.call(rbind.data.frame, etiquetas_abiertas)
  open_issues_asignaciones <- data.frame(etiquetas_abiertas,  open_issues_asignaciones)
  open_issues_labels_asignaciones <- open_issues_asignaciones %>%
    select(name, state, created_at, updated_at)

  # Unir todos los issues abierto
  open_issues_labels <- rbind(open_issues_labels_asignaciones,
                              open_issues_labels_sitio)

  # Incidentes cerrados sitio_pruebas
  closed_issues_sitio <- gitear::get_issues_closed_state(
    base_url = "https://gitear.ixpantia.com/",
    api_key = "47c0be813944aaa0132d77a8110d48e9d3a644af",
    owner = "ixplorer",
    repo = "sitio_pruebas")

  closed_issues_sitio <- jsonlite::flatten(closed_issues_sitio)
  etiquetas_cerradas <- closed_issues_sitio$labels
  etiquetas_cerradas <- do.call(rbind.data.frame, etiquetas_cerradas)
  closed_issues_sitio <- data.frame(etiquetas_cerradas,  closed_issues_sitio)
  closed_issues_labels_sitio <- closed_issues_sitio %>%
    select(name, state, created_at, updated_at)

  # Incidentes cerrados asignaciones
  closed_issues_asignaciones <- gitear::get_issues_closed_state(
    base_url = "https://gitear.ixpantia.com/",
    api_key = "47c0be813944aaa0132d77a8110d48e9d3a644af",
    owner = "ixplorer",
    repo = "asignaciones")

  closed_issues_asignaciones <- jsonlite::flatten(closed_issues_asignaciones)
  etiquetas_cerradas <- closed_issues_asignaciones$labels
  etiquetas_cerradas <- do.call(rbind.data.frame, etiquetas_cerradas)
  closed_issues_asignaciones <- data.frame(etiquetas_cerradas,  closed_issues_asignaciones)
  closed_issues_labels_asignaciones <- closed_issues_asignaciones %>%
    select(name, state, created_at, updated_at)

  # Unir todos los issues cerrados
  closed_issues_labels <- rbind(closed_issues_labels_asignaciones,
                              closed_issues_labels_sitio)

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
      layout(barmode = "stack") %>%
      config(displayModeBar = FALSE)
    p1
  })

  output$plot2 <- renderPlot({
   plot(project_data)
  })
}

