project_UI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(6, plotlyOutput(ns("plot1"))),
    column(6, plotlyOutput(ns("plot2")))
  )

}

project <- function(input, output, session,
                    project_data = "No Projects") {

  # Loop traer todos los datos de los repositorios existentes

  repos <- gitear::get_list_repos_org(
    base_url = Sys.getenv("IXURL"),
    api_key = Sys.getenv("IXTOKEN"),
    org = Sys.getenv("IXPROJECT"))

  open_repos_list <- list()

  for (name_repo in repos$name) {
      open_repos_list[[name_repo]] <- gitear::get_issues_open_state(
      base_url = Sys.getenv("IXURL"),
      api_key = Sys.getenv("IXTOKEN"),
      owner = Sys.getenv("IXPROJECT"),
      repo = name_repo) %>%
        jsonlite::flatten()
  }

  open_repositories <- do.call(rbind.data.frame, open_repos_list) %>%
    tibble::rownames_to_column() %>%
    tidyr::separate(col = rowname, into  = c("repo", "ba"), sep = "\\.") %>%
    dplyr::select(-ba)

  closed_repos_list <- list()

  for (name_repo in repos$name) {
    closed_repos_list[[name_repo]] <- gitear::get_issues_closed_state(
      base_url = Sys.getenv("IXURL"),
      api_key = Sys.getenv("IXTOKEN"),
      owner = Sys.getenv("IXPROJECT"),
      repo = name_repo) %>%
      jsonlite::flatten()
  }

  closed_repositories <- do.call(rbind.data.frame, closed_repos_list) %>%
    tibble::rownames_to_column() %>%
    tidyr::separate(col = rowname, into  = c("repo", "ba"), sep = "\\.") %>%
    dplyr::select(-ba)

  repositories <- rbind(open_repositories, closed_repositories)

  ## Esto tiene que ir aplicado a todos: ------

  ### Tener cuidado que este segmento se pega por orden.
  etiqueta <- repositories$labels
  etiqueta <- do.call(cbind.data.frame, etiquetas) %>%
    select(name) %>%
    rename(etiqueta =   name)

  repositories <- cbind(repositories, etiqueta)

  etiquetas <- data.frame(name = character(0),
                                   stringsAsFactors = FALSE)

  for (i in seq_along(repositories$id)) {
    # TODO: #80
    etiquetas_repos <- repositories$labels[[i]]$name[1]
    if (is.null(etiqueta)) { etiqueta <- NA }
    etiquetas[i,1] <- etiquetas_repos
  }

  incidentes <- data.frame(etiquetas, repositories) %>%
    select(name, state, created_at, updated_at) %>%
    mutate(created_at = lubridate::ymd_hms(created_at)) %>%
    mutate(updated_at = lubridate::ymd_hms(updated_at))

  int = interval(today() - 7, today() + 1)

  incidentes <- incidentes %>%
    mutate(state = ifelse(created_at %within% int, "last", incidentes$state))

# -------------------------------------------------------------------------

  # Seleccion open issues para cummmulative flow chart
  open_issues <- rbind(open_issues_asignaciones, open_issues_sitio)
  open_issues_assignee <- open_issues %>%
    select(state, created_at, updated_at, assignee.username) %>%
    mutate(category = ifelse(is.na(open_issues$assignee.username),
                             "open_unassigned", "open_assigned"))

  #  Seleccion closed issues para cummulative flow chart
  closed_issues <- rbind(closed_issues_asignaciones,  closed_issues_sitio)
  closed_issues_assignee <- closed_issues %>%
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
    p1
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
}

