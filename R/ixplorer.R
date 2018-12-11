#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import kableExtra
NULL

#' ixplorer reports
#'
#' Visualize the issues of an specific user, a team and get the quick links to
#' your ixplorer based on the credentials used in gadget authenticate.
#'
#' @export
ix_issues <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer Reports"),
    miniTabstripPanel(
      miniTabPanel("My issues", icon = icon("user"),
                   miniContentPanel(
                     tableOutput("my_issues")
                   )
      ),
      miniTabPanel("Team issues", icon = icon("users"),
                   miniContentPanel(
                     tableOutput("team_issues")
                   )
      ),
      miniTabPanel("Quick links", icon = icon("link"),
                   miniContentPanel(
                     tableOutput("quick_links")
                   )
      )
    )
  )

  server <- function(input, output, session){

    # Verificar/configurar datos de autentificacion
    access_file <- verify_ixplorer_file()
    set_authentication(access_data = access_file)

    if (Sys.getenv("IXTOKEN") == "") {
      print("no hay IXTOKEN")
    }

    if (Sys.getenv("IXURL") == "") {
      print("no hay IXURL")
    }

    if (Sys.getenv("IXOWNER") == "") {
      print("no hay IXOWNER")
    }

    if (Sys.getenv("IXREPO") == "") {
      print("no hay IXREPO")
    }

    if (Sys.getenv("IXUSER") == "") {
      print("no hay IXUSER")
    }

    # Traemos issues y configuramos credenciales
    issues <- gitear::get_issues_open_state(base_url = Sys.getenv("IXURL"),
                                 api_key = Sys.getenv("IXTOKEN"),
                                 owner = Sys.getenv("IXOWNER"),
                                 repo = Sys.getenv("IXREPO"))
    ixplorer_user = Sys.getenv("IXUSER")

    # Desanidar cuadro
    issues <- jsonlite::flatten(issues)

    output$my_issues <- function() {
      # Seleccion de issues por usuario y creacion links de issues
      issues <- issues %>%
        filter(assignee.login == ixplorer_user) %>%
        select(number, title, due_date, url) %>%
        tidyr::separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = lubridate::ymd(due_date) - lubridate::today()) %>%
        tidyr::separate(col = url,
                 into = c("borrar", "issue_url"), sep = "repos/") %>%
        select(-borrar) %>%
        mutate(issue_url = paste(Sys.getenv("IXURL"), issue_url, sep = ""))

      issues <- rename(issues, Title = title)
      issues <- rename(issues, Nr = number)
      issues <- rename(issues, Due = due_date)

      verdes <- RColorBrewer::brewer.pal(nrow(issues), "Greens")
      rojos <- RColorBrewer::brewer.pal(nrow(issues), "Reds")

      issues_kable <- issues %>%
        mutate(Due = ifelse(Due < 0, cell_spec(Due, color = "white",
                                       bold = TRUE, background = rojos),
                                   cell_spec(Due, color = "white",
                                       bold = TRUE, background = verdes)),
               Nr = text_spec(Nr, link = issue_url)) %>%
        select(-issue_url) %>%
        kable(escape = FALSE) %>%
        kable_styling("striped", "condensed")

      return(issues_kable)
    }

    output$team_issues <- function(){
      # Seleccionamos issues por estado abierto
      issues <- issues %>%
        select(user.login, number, title, due_date, url) %>%
        tidyr::separate(col = due_date, into = c("due_date", "hour"),
                        sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = lubridate::ymd(due_date) - lubridate::today()) %>%
        mutate(due_date = as.numeric(due_date)) %>%
        tidyr::separate(col = url,
                 into = c("borrar", "issue_url"), sep = "repos/") %>%
        select(-borrar) %>%
        mutate(issue_url = paste(Sys.getenv("IXURL"), issue_url, sep = ""))

      issues <- rename(issues, Title = title)
      issues <- rename(issues, Nr = number)
      issues <- rename(issues, Due = due_date)
      issues <- rename(issues,  User = user.login)

      verdes <- RColorBrewer::brewer.pal(nrow(issues), "Greens")
      rojos <- RColorBrewer::brewer.pal(nrow(issues), "Reds")

      issues_kable <- issues %>%
        mutate(
          Due = ifelse(Due < 0,
                                   cell_spec(Due, color = "white",
                                             bold = TRUE, background = rojos),
                                   cell_spec(Due, color = "white",
                                             bold = TRUE, background = verdes)),
               User = cell_spec(User,
                                bold = ifelse(ixplorer_user == User, TRUE, FALSE),
                                color = ifelse(ixplorer_user  == User,
                                               "gray", "black")),
               Nr = text_spec(Nr, link = issue_url)) %>%
        select(-issue_url) %>%
        kable(escape = FALSE) %>%
        kable_styling("striped", "condensed")

      return(issues_kable)

    }

    output$quick_links <- function(){
      # Traer link de closed issues
      close_issues_url <- "issues?q=&type=all&sort=&state=closed&labels=0&milestone=0&assignee=0"
      ixurl <- sub("/$", "", Sys.getenv("IXURL"))
      close_issues_url <- paste(ixurl, Sys.getenv("IXOWNER"), Sys.getenv("IXREPO"),
            close_issues_url, sep = "/")

      # Link de milestones
      milestones_url <- paste(ixurl, Sys.getenv("IXOWNER"),
                              Sys.getenv("IXREPO"), "milestones", sep = "/")

      # Link de Wiki
      wiki_url <- paste(ixurl, Sys.getenv("IXOWNER"),
                        Sys.getenv("IXREPO"), "wiki", sep = "/")

      # Link de proyecto
      project_url <- paste(ixurl, Sys.getenv("IXOWNER"), sep = "/")

      links <- c(close_issues_url, milestones_url, wiki_url, project_url)
      URL <- c("Clossed issues", "Milestones", "Wiki", "Project")

      quick_links <- data_frame(links, URL)

      quick_links <- quick_links %>%
        mutate(
          URL = text_spec(URL, link = links)) %>%
        select(-links) %>%
        kable(escape = FALSE, align = "c") %>%
        kable_styling("striped", "condensed", position = "center",
                      font_size = 20)

      return(quick_links)
    }

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    observeEvent(input$cancel, {
      stopApp(TRUE)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))

}



