repository_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6, plotlyOutput(ns("plot_bar_tickets"))),
      column(6, plotlyOutput(ns("plot_cumflow_tickets")))
    ),

    fluidRow(
      shinydashboard::box(title = "Histogram box title",
          status = "warning", solidHeader = TRUE, collapsible = TRUE,
          plotlyOutput(ns("plot_commits_repos"))
      ),
      # column(6, plotlyOutput(ns("plot3"))),
      column(6, plotlyOutput(ns("plot_commits_person")))
    )
  )

}

repository <- function(input, output, session,
                         repo_name, project_name) {

  # Incidentes abiertos OPEN --------------------------
  open_tickets <- data.frame(character(0))

  try(
  open_tickets <- gitear::get_issues_open_state(
    base_url = Sys.getenv("IXURL"),
    api_key = Sys.getenv("IXTOKEN"),
    owner = project_name,
    repo = repo_name)
  )

  if (nrow(open_tickets) == 0) {
  }  else {
    open_tickets <- jsonlite::flatten(open_tickets)
    # Aplastar labels
    # etiquetas_abiertas <- open_tickets$labels

    etiquetas <- data.frame(name = character(0),
                            stringsAsFactors = FALSE)

    # Loop para elegir primera etiqueta OPEN tickets
    for (i in seq_along(open_tickets$labels)) {
      # TODO: #80
      etiqueta <- open_tickets$labels[[i]]$name[1]
      if (is.null(etiqueta)) { etiqueta <- "Not labeled" }
      etiquetas[i,1] <- etiqueta
    }

    # Unir a todo el conjunto de datos
    open_tickets <- data.frame(etiquetas,  open_tickets)

    # Seleccion columnas  necesarias OPEN_tickets
    open_tickets_labels <- open_tickets %>%
      select(name, state, created_at, updated_at)
  }

  # Incidentes cerrados CLOSED --------------------------
  closed_tickets <- data.frame(character(0))
  try(
  closed_tickets <- gitear::get_issues_closed_state(
    base_url = Sys.getenv("IXURL"),
    api_key = Sys.getenv("IXTOKEN"),
    owner = project_name,
    repo = repo_name)
  )

  if (nrow(closed_tickets) == 0) {
  }  else {
    closed_tickets <- jsonlite::flatten(closed_tickets)

    etiquetas <- data.frame(name = character(0),
                            stringsAsFactors = FALSE)

    # Loop para elegir primera etiqueta de CLOSED tickets
    for (i in seq_along(closed_tickets$labels)) {
      # TODO: #80
      etiqueta <- closed_tickets$labels[[i]]$name[1]
      if (is.null(etiqueta)) { etiqueta <- "Not labeled" }
      etiquetas[i,1] <- etiqueta
    }

    # Unir labels a todo el conjunto de datos
    closed_tickets <- data.frame(etiquetas,  closed_tickets)

    # Seleccion columnas  necesarias CLOSED_tickets
    closed_tickets_labels <- closed_tickets %>%
      select(name, state, created_at, updated_at)
  }

  # Union de incidentes cerrados/abiertos con etiquetas -----------------------
  # y formato de fechas:

  if (nrow(open_tickets) == 0 & nrow(closed_tickets) == 0) {

    # Hacer aqui un conjunto de datos con 0's con columna name e
    # incidentes
    incidentes <- data_frame("name" = NA, "incidentes" = NA,
                             "state" = NA)
  } else if (nrow(closed_tickets) == 0) {

    int = interval(today() - 7, today() + 1) #Esto porque no agarra el ultimo
    incidentes <- open_tickets_labels %>%
      mutate(created_at = lubridate::ymd_hms(created_at)) %>%
      mutate(updated_at = lubridate::ymd_hms(updated_at))

    incidentes <- incidentes %>%
      mutate(state = ifelse(created_at %within% int, "last", incidentes$state))
    } else if (nrow(open_tickets) == 0) {

      int = interval(today() - 7, today() + 1) #Esto porque no agarra el ultimo

      incidentes <- closed_tickets_labels %>%
        mutate(created_at = lubridate::ymd_hms(created_at)) %>%
        mutate(updated_at = lubridate::ymd_hms(updated_at))

      incidentes <- incidentes %>%
        mutate(state = ifelse(created_at %within% int, "last", incidentes$state))
    } else {
    int = interval(today() - 7, today() + 1) #Esto porque no agarra el ultimo

    incidentes <- rbind(closed_tickets_labels, open_tickets_labels) %>%
      mutate(created_at = lubridate::ymd_hms(created_at)) %>%
      mutate(updated_at = lubridate::ymd_hms(updated_at))

    incidentes <- incidentes %>%
      mutate(state = ifelse(created_at %within% int, "last", incidentes$state))
  }


  # Seleccion open tickets para cummmulative flow chart ------------------------
  if (nrow(open_tickets) == 0) {
    cat("There are no open tickets in ", project_name, "::", repo_name, "\n")
  } else {
    if ("assignee.username" %notin% names(open_tickets)) {
      open_tickets$assignee.username <- NA
    }
    open_tickets_assignee <- open_tickets %>%
      select(state, created_at, updated_at, assignee.username) %>%
      mutate(category = ifelse(is.na(open_tickets$assignee.username),
                               "open_unassigned", "open_assigned"))
  }

  #  Seleccion closed tickets para cummulative flow chart
  if (nrow(closed_tickets) == 0) {
    cat("There are no closed tickets in ", project_name, "::", repo_name, "\n")
  } else {
    if ("assignee.username" %notin% names(closed_tickets)) {
      closed_tickets$assignee.username <- NA
    }
    closed_tickets_assignee <- closed_tickets %>%
      select(state, created_at, updated_at, assignee.username) %>%
      mutate(category = ifelse(is.na(closed_tickets$assignee.username),
                               "closed_unassigned", "closed_assigned"))
    }

  # Asignados completos:
  if (nrow(open_tickets) == 0 & nrow(closed_tickets) == 0) {
    cum_flow_chart_data <- data_frame("date" = lubridate::today())
  } else if (nrow(closed_tickets) == 0) {
    asignados <- open_tickets_assignee %>%
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
  } else if (nrow(open_tickets) == 0) {
    asignados <- closed_tickets_assignee %>%
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
  } else {
    asignados <- rbind(open_tickets_assignee, closed_tickets_assignee) %>%
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
  }

  # Condicionales columnas que no xisten con ceros
  if ("open_assigned"  %notin% names(cum_flow_chart_data)) {
    cum_flow_chart_data$open_assigned <- 0
  }

  if ("open_unassigned" %notin% names(cum_flow_chart_data)) {
    cum_flow_chart_data$open_unassigned <- 0
  }

  if ("closed_assigned" %notin% names(cum_flow_chart_data)) {
    cum_flow_chart_data$closed_assigned <- 0
  }

  if ("closed_unassigned" %notin% names(cum_flow_chart_data)) {
    cum_flow_chart_data$closed_unassigned <- 0
  }

  # Suma acumulativa para cada una de las columnas:
  cum_flow_chart_data <- cum_flow_chart_data %>%
    ungroup() %>%
    mutate(open_assigned = cumsum(open_assigned)) %>%
    mutate(open_unassigned = cumsum(open_unassigned)) %>%
    mutate(closed_assigned = cumsum(closed_assigned)) %>%
    mutate(closed_unassigned = cumsum(closed_unassigned))

  cum_flow_chart_data$date <- as.factor(cum_flow_chart_data$date)

  # PLOTS ----------------------------------------------------------------------
  output$plot_bar_tickets <- renderPlotly({
    p1 <- plot_ly(incidentes, y = ~ name, color = ~ state,
                  colors = c("grey50", "slateblue")) %>%
      add_histogram() %>%
      layout(barmode = "stack") %>%
      plotly::config(displayModeBar = FALSE)
    return(p1)
    # suppressWarnings(print(p1))
  })

  output$plot_cumflow_tickets <- renderPlotly({
    p <- plotly::plot_ly(cum_flow_chart_data, x = ~date, y = ~closed_assigned,
                         name = "Closed assigned", type = 'scatter', mode = 'none',
                         fillcolor = '#0078B4') %>%
      add_trace(y = ~closed_unassigned, name = "Closed unassigned",
                fillcolor = '#A78D7B') %>%
      add_trace(y = ~open_assigned, name = "Open assigned",
                fillcolor = '#F8A212') %>%
      add_trace(y = ~open_unassigned, name = "Open unassigned",
                fillcolor = '#2A2A2A') %>%
      layout(title = "tickets categories for ixplorer repo_pruebas",
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "tickets total",
                          showgrid = FALSE)) %>%
      plotly::config(displayModeBar = FALSE)

    return(p)

  })

  output$plot_commits_repos <- renderPlotly({

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

    p1 <- plotly::plot_ly(commits_repo, x = ~date, y = ~asignaciones,
                    name = "asignaciones", type = 'scatter', mode = 'none',
                    fillcolor = '#0078B4') %>%
                    #stackgroup  = 'one', fillcolor = '#0078B4') %>%
      add_trace(y = ~sitio_pruebas, name = "sitio_pruebas",
                fillcolor = '#A78D7B') %>%
      layout(title = 'commits total on ixplorer',
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "commits total",
                          showgrid = FALSE)) %>%
      plotly::config(displayModeBar = FALSE)

    return(p1)
    # suppressWarnings(print(p1))

  })

  output$plot_commits_person <- renderPlotly({
    # commits por persona por repositorio barras
    commits_person <- readxl::read_xlsx("data/commits_person.xlsx")

    # Crear los intervalos de semana y mes
    int_week <- interval(today() - 7, today() + 1) #Esto porque no agarra el ultimo
    int_month <- interval(today() - 37, today() - 7) #Esto porque no agarra el ultimo

    # Clasificacion de commmits en mes semana o mas antiguo
    commits_person <- commits_person %>%
      mutate(state = ifelse(date %within% int_month, "month",
                            ifelse(date %within% int_week, "week", "older")))

    p1 <- plot_ly(commits_person, y = ~ person, color = ~ state,
                  colors = c("darkred", "gray")) %>%
      add_histogram() %>%
      layout(title = 'commits ixplorer per person',
             barmode = "stack") %>%
      plotly::config(displayModeBar = FALSE)

    return(p1)

  })

}

