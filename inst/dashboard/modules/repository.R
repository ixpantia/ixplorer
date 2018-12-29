repository_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6, plotlyOutput(ns("plot1"))),
      column(6, plotlyOutput(ns("plot2")))
    ),

    fluidRow(
      shinydashboard::box(title = "Histogram box title",
          status = "warning", solidHeader = TRUE, collapsible = TRUE,
          plotlyOutput(ns("plot3"))
      ),
      # column(6, plotlyOutput(ns("plot3"))),
      column(6, plotlyOutput(ns("plot4")))
    )

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

  # Seleccion open issues para cummmulative flow chart
  open_issues_assignee <- open_issues %>%
    select(state, created_at, updated_at, assignee.username) %>%
    mutate(category = ifelse(is.na(open_issues$assignee.username),
                             "open_unassigned", "open_assigned"))

  #  Seleccion closed issues para cummulative flow chart
  closed_issues_assignee <-closed_issues %>%
    select(state, created_at, updated_at, assignee.username) %>%
    mutate(category = ifelse(is.na(closed_issues$assignee.username),
                             "closed_unassigned", "closed_assigned"))

  # Asignados completos:
  asignados <- rbind(open_issues_assignee, closed_issues_assignee) %>%
    mutate(created_at = lubridate::ymd_hms(created_at)) %>%
    mutate(updated_at = lubridate::ymd_hms(updated_at))

  # Para este cummulative flow chart necesito darle vuelta a los datos
  # agrupados por fecha y cada una de las variables
  cum_flow_chart_data <- asignados %>%
    group_by(lubridate::date(created_at), category) %>%
    summarise(
      total = n()
    ) %>%
    rename(
      date = `lubridate::date(created_at)`
    )

  # Ahora toca darle vuelta:
  cum_flow_chart_data <- tidyr::spread(data = cum_flow_chart_data,
                                       key = category, value = total)
  # replace_na
  cum_flow_chart_data[is.na(cum_flow_chart_data)] <- 0

  cum_flow_chart_data <- cum_flow_chart_data %>%
    ungroup() %>%
    mutate(open_assigned = cumsum(open_assigned)) %>%
    mutate(open_unassigned = cumsum(open_unassigned)) %>%
    mutate(closed_assigned = cumsum(closed_assigned)) %>%
    mutate(closed_unassigned = cumsum(closed_unassigned))

  cum_flow_chart_data$date <- as.factor(cum_flow_chart_data$date)

  output$plot1 <- renderPlotly({
    p1 <- plot_ly(incidentes, y = ~ name, color = ~ state) %>%
      add_histogram() %>%
      layout(barmode = "stack") %>%
      plotly::config(displayModeBar = FALSE)
    return(p1)
  })

  output$plot2 <- renderPlotly({
    p <- plotly::plot_ly(cum_flow_chart_data, x= ~date, y = ~closed_assigned,
                         name = "Closed assigned", type = 'scatter', mode = 'none',
                         stackgroup  = 'one', fillcolor = '#0078B4') %>%
      add_trace(y = ~closed_unassigned, name = "Closed unassigned",
                fillcolor = '#A78D7B') %>%
      add_trace(y = ~open_assigned, name = "Open assigned",
                fillcolor = '#F8A212') %>%
      add_trace(y = ~open_unassigned, name = "Open unassigned",
                fillcolor = '#2A2A2A') %>%
      layout(title = 'Issues categories for ixplorer repo_pruebas',
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "Issues total",
                          showgrid = FALSE)) %>%
      plotly::config(displayModeBar = FALSE)
    p
  })

  output$plot3 <- renderPlotly({
    # Commits por repositorios de un proyecto
    commits_repo <- readxl::read_xlsx("data/commits_repos.xlsx")
    # Ahora toca darle vuelta:
    commits <- tidyr::spread(data = commits_repo,
                             key = repository, value = commits)
    # replace_na
    commits[is.na(commits)] <- 0

    commits_repo <- commits %>%
      ungroup() %>%
      mutate(asignaciones = cumsum(asignaciones)) %>%
      mutate(sitio_pruebas = cumsum(sitio_pruebas))

    plotly::plot_ly(commits_repo, x= ~date, y = ~asignaciones,
                    name = "asignaciones", type = 'scatter', mode = 'none',
                    stackgroup  = 'one', fillcolor = '#0078B4')%>%
      add_trace(y = ~sitio_pruebas, name = "sitio_pruebas",
                fillcolor = '#A78D7B') %>%
      layout(title = 'commits total on ixplorer',
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "commits total",
                          showgrid = FALSE)) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$plot4 <- renderPlotly({
    # commits por persona por repositorio barras
    commits_project <- readxl::read_xlsx("data/commits_person.xlsx")

    # Crear los intervalos de semana y mes
    int_week <- interval(today() - 7, today() + 1) #Esto porque no agarra el ultimo
    int_month <- interval(today() - 37, today() - 7) #Esto porque no agarra el ultimo

    commits_person <- commits_person %>%
      mutate(state = ifelse(date %within% int_month, "month",
                            ifelse(date %within% int_week, "week", "older")))

    p1 <- plot_ly(commits_person, y = ~ person, color = ~ state) %>%
      add_histogram() %>%
      layout(title = 'commits ixplorer per person',
             barmode = "stack") %>%
      plotly::config(displayModeBar = FALSE)

    p1
  })

}

