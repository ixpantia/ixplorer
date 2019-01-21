project_UI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(6, plotlyOutput(ns("plot1"))),
    column(6, plotlyOutput(ns("plot2")))
  )

}

project <- function(input, output, session,
                    project_name) {

  # Traigo nombres de repositorios existentes

  repos <- tryCatch(
    {
      (gitear::get_list_repos_org(
        base_url = Sys.getenv("IXURL"),
        api_key = Sys.getenv("IXTOKEN"),
        org = project_name))
    },
    error = function(cond){
      stopApp(TRUE)
      print("Invalid credentials")
    }
  )

  # Loop traer todos los datos de open_tickets de los repositorios existentes

    open_repos_list <- list()
    for (name_repo in repos$name) {
      try(
        open_repos_list[[name_repo]] <- gitear::get_issues_open_state(
        base_url = Sys.getenv("IXURL"),
        api_key = Sys.getenv("IXTOKEN"),
        owner = project_name,
        repo = name_repo) %>%
          jsonlite::flatten(.)
      )
    }


  for (repo in names(open_repos_list)) {
    n_elements <- length(open_repos_list[repo][[1]]$id)
    if (n_elements == 0 ) {
      open_repos_list[repo] <- NULL
      } else {
      open_repos_list[repo][[1]]$repo <- rep(repo, n_elements)
      }
  }

  open_tickets <-  open_repos_list %>%
    bind_rows()

  if ("assignee.id" %notin% names(open_tickets)) {
    open_tickets <- open_tickets
  } else {
    open_tickets <- open_tickets %>%
    dplyr::select(-assignee.id, -assignee.login, -assignee.full_name,
                  -assignee.email, -assignee.avatar_url, -assignee.language)
  }

 if ("assignee.username" %notin% names(open_tickets)) {
    open_tickets$assignee.username <- NA
  }
  if ("assignee" %notin% names(open_tickets)) {
    open_tickets$assignee <- NA
  }
  # Loop traer todos los datos de CLOSED_tickets de los repositorios existentes
  closed_repos_list <- list()
  for (name_repo in repos$name) {
    try(
      closed_repos_list[[name_repo]] <- gitear::get_issues_closed_state(
        base_url = Sys.getenv("IXURL"),
        api_key = Sys.getenv("IXTOKEN"),
        owner = project_name,
        repo = name_repo) %>%
        jsonlite::flatten(.)
    )
  }

  for (repo in names(closed_repos_list)) {
    n_elements <- length(closed_repos_list[repo][[1]]$id)
    closed_repos_list[repo][[1]]$repo <- rep(repo, n_elements)
  }

  # Convertir CLOSED_tickets de todos los repos en tidydata
  if (nrow(closed_repos_list[[1]]) > 1) {
  closed_tickets <- closed_repos_list %>%
    bind_rows() %>%
    dplyr::select( -assignee.id, -assignee.login, -assignee.full_name,
                  -assignee.email, -assignee.avatar_url, -assignee.language)


    if ("assignee.username" %notin% names(closed_tickets)) {
      closed_tickets$assignee.username <- NA
    }
    if ("assignee" %notin% names(closed_tickets)) {
      closed_tickets$assignee <- NA
    }
    # Unir OPEN_tickets con CLOSED_tickets
    repositories <- rbind(open_tickets, closed_tickets)
  } else {
    repositories <- open_tickets
    }
  ## Terminar de limpiar los datos de incidentes (abiertos y cerrados)

  # Loop para poner NA si no hay etiqueta
  etiquetas <- data.frame(name = character(0),
                                   stringsAsFactors = FALSE)

  for (i in seq_along(repositories$id)) {
    # TODO: #80
    etiqueta <- repositories$labels[[i]]$name[1]
    if (is.null(etiqueta)) { etiqueta <- "Not labeled" }
    etiquetas[i,1] <- etiqueta
  }

  # Seleccionar columnas deseadas y formato de fechas
  incidentes <- data.frame(etiquetas, repositories) %>%
    select(name, state, created_at, updated_at) %>%
    mutate(created_at = lubridate::ymd_hms(created_at)) %>%
    mutate(updated_at = lubridate::ymd_hms(updated_at))

  # Crear intervalo para clasificar incidentes de la ultima semana
  int = lubridate::interval(lubridate::today() - 7, lubridate::today() + 1)

  # hacer clasficacion de incidentes basados en intervalo
  incidentes <- incidentes %>%
    mutate(state = ifelse(created_at %within% int, "last", incidentes$state))

# -------------------------------------------------------------------------

  # Seleccion open tickets para cummmulative flow chart
  # Crear condicional que diga si esta abierto asignado, abierto sin
  # asignar, cerrado asignado, cerrado sin asignar
  asignados <- repositories %>%
    select(state, created_at, updated_at, assignee.username) %>%
    mutate(category =
             ifelse(state == "open",
                    ifelse(is.na(repositories$assignee.username),
                           "open_unassigned", "open_assigned"),
                    ifelse(is.na(repositories$assignee.username),
                           "closed_unassigned", "closed_assigned")
                    )
    ) %>%
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

  output$plot1 <- renderPlotly({
    p1 <- plotly::plot_ly(incidentes, y = ~ name, color = ~ state,
                          colors = c("#F8A212", "#A78D7B")) %>%
      plotly::add_histogram() %>%
      plotly::layout(title = "Tickets categories per status",
                     barmode = "stack") %>%
      plotly::config(displayModeBar = FALSE)

    return(p1)
    # suppressWarnings(print(p1))
  })

  output$plot2 <- renderPlotly({
    p <- plotly::plot_ly(cum_flow_chart_data, x = ~date, y = ~closed_assigned,
                         name = "Closed assigned", type = 'scatter', mode = 'none',
                         stackgroup  = 'one', fillcolor = '#0078B4') %>%
      plotly::add_trace(y = ~closed_unassigned, name = "Closed unassigned",
                fillcolor = '#A78D7B') %>%
      plotly::add_trace(y = ~open_assigned, name = "Open assigned",
                fillcolor = '#F8A212') %>%
      plotly::add_trace(y = ~open_unassigned, name = "Open unassigned",
                fillcolor = '#FFCF4D') %>%
      plotly::layout(title = 'Cummulative flow chart for tickets status',
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "tickets total",
                          showgrid = FALSE)) %>%
      plotly::config(displayModeBar = FALSE)
    p
  })

}

