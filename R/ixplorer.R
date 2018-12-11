#' @import shiny
#' @import miniUI
#' @import DT
#' @import gitear
#' @import dplyr
#' @import jsonlite
#' @import kableExtra
#' @import lubridate
#' @import tidyr
#' @import RColorBrewer
NULL

#' ixplorer reports
#'
#' Visualize the issues of an specific user, a team and closed issues based on
#' the credentials used in gadget authenticate.
#'
#' @export
ix_issues <- function() {

  ui <- miniPage(
    gadgetTitleBar("ixplorer Reports"),
    verbatimTextOutput("warning", placeholder = FALSE),
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
      miniTabPanel("Closed issues", icon = icon("times-circle"),
                   miniContentPanel(
                     DT::dataTableOutput("closed_issues")
                   )
      )
    )
  )

  server <- function(input, output, session){

    access_file <- verify_ixplorer_file()

    output$warning <- renderText({

      a <- if(access_file == "no access data"){
        print(access_file)
      } else {
        set_authentication(access_data = access_file)
      }
      return(a)
    })

    # Traemos issues y configuramos credenciales
    issues <- gitear::get_issues_open_state(base_url = Sys.getenv("IXURL"),
                                 api_key = Sys.getenv("IXTOKEN"),
                                 owner = Sys.getenv("IXOWNER"),
                                 repo = Sys.getenv("IXREPO"))
    ixplorer_user = Sys.getenv("IXUSER")

    # Desanidar cuadro
    issues <- flatten(issues)

    output$my_issues <- function() {
      # Seleccion de issues por usuario y creacion links de issues
      issues <- issues %>%
        filter(assignee.login == ixplorer_user) %>%
        select(number, title, due_date, url) %>%
        separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = ymd(due_date) - today()) %>%
        separate(col = url,
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
        separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
        select(-hour) %>%
        mutate(due_date = ymd(due_date) - today()) %>%
        mutate(due_date = as.numeric(due_date)) %>%
        separate(col = url,
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

    output$closed_issues <- DT::renderDataTable({
      # Traer issues que estan cerrados. TODO
      issues_closed <- gitear::get_issues_closed_state(base_url = Sys.getenv("IXURL"),
                                        api_key = Sys.getenv("IXTOKEN"),
                                        owner = Sys.getenv("IXOWNER"),
                                        repo = Sys.getenv("IXREPO")
      ) %>%
        select(title, body, due_date, labels)
      issues_closed <- flatten(issues_closed)
      return(issues_closed)
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    observeEvent(input$cancel, {
      stopApp(TRUE)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))

}



