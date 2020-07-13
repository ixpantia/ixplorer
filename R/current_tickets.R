#' @import shiny
#' @import miniUI
NULL

#' @title Tiquetes actuales
#' @description Visualice los tiquetes de un usuario en específico, de un equipo
#'  y obtenga los enlaces a su ixplorer basado en las credenciales utilizadas en el
#' gadget de autentificación.
#'
#' @param instance instancia de ixplorer (Ejm: "secure", "masterclass", "prueba")
#' @param owner el nombre del proyecto donde se encuentra el repositorio en
#' ixplorer
#' @param repository el nombre del repositorio donde están los tiquetes
#'
#' @export
current_tickets <- function(instance, owner, repository = "current") {

  if(repository == "current") {
    repository <- basename(rstudioapi::getActiveProject())
  }

  ui <- miniPage(
    miniTitleBar("Tiquetes actuales",
                 right = miniTitleBarCancelButton(inputId = "done",
                                                 label = "Listo",
                                                 primary = TRUE)
                 ),
    verbatimTextOutput("warning", placeholder = FALSE),
    miniTabstripPanel(
      miniTabPanel("Mis tiquetes", icon = icon("user"),
                   miniContentPanel(
                     tableOutput("my_tickets")
                   )
      ),
      miniTabPanel("Tiquetes del equipo", icon = icon("users"),
                   miniContentPanel(
                     tableOutput("team_tickets")
                   )
      ),
      miniTabPanel("Enlaces", icon = icon("link"),
                   miniContentPanel(
                     tableOutput("quick_links")
                   )
      )
    )
  )

  server <- function(input, output, session){

    credenciales <- tryCatch(
      keyring::key_get(paste0("token_", instance)),
      error = function(cond) "no_credenciales")


    if(credenciales != "no_credenciales") {
      credenciales <- credenciales %>%
        stringr::str_split("/", simplify = TRUE) %>%
        tibble::as_tibble() %>%
        magrittr::set_names(c("url", "token",
                              "usuario", "persistencia")) %>%
        dplyr::mutate(persistencia = as.logical(persistencia))

    }

    output$warning <- renderText({
      msg <- ifelse (credenciales == "no_credenciales",
                     "No hay archivo de credenciales disponible", "")
      return(msg)
    })

    # Get tickets and configurate credentials
    tickets <- tryCatch({

      gitear::get_issues_open_state(
           base_url = paste0("https://", credenciales$url),
           api_key = credenciales$token,
           owner = owner,
           repo = repository)

      },
      error = function(cond) {
        tickets <- "Invalid"
      })



    output$my_tickets <- function() {
      if (class(tickets) != "data.frame") {
        tickets_kable <- "Credenciales inválidas. Por favor use el gadget de autentificación."
      } else if (nrow(tickets) == 0) {
        tickets_kable <- "No se encontraron tiquetes en el repositorio."
      } else {
        # Select tickets by user and tickets link creation
        tickets <- tickets %>%
          dplyr::filter(assignee.login == credenciales$usuario) %>%
          dplyr:: select(number, title, due_date, url) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"),
                          sep = "T") %>%
          dplyr::select(-hour) %>%
          dplyr::mutate(due_date = lubridate::ymd(due_date) -
                          lubridate::today()) %>%
          tidyr::separate(col = url,
                   into = c("borrar", "issue_url"), sep = "repos/") %>%
          dplyr::select(-borrar) %>%
          dplyr::mutate(
            issue_url = paste(base_url = paste0("https://", credenciales$url),
                              issue_url, sep = "/")) %>%
          dplyr::arrange(desc(due_date)) %>%
          dplyr::rename(Title = title,  Nr = number, Due = due_date)

        suppressWarnings(verdes <- RColorBrewer::brewer.pal(nrow(tickets), "Greens"))
        suppressWarnings(rojos <- RColorBrewer::brewer.pal(nrow(tickets), "Reds"))

        tickets_kable <- tickets %>%
          dplyr::mutate(Due = ifelse(Due < 0, kableExtra::cell_spec(Due, color = "white",
                                                 bold = TRUE, background = rojos),
                              kableExtra::cell_spec(Due, color = "white",
                                        bold = TRUE, background = verdes)),
                 Nr = kableExtra::text_spec(Nr, link = issue_url),
                 Due = ifelse(is.na(Due), "-", Due)) %>%
          dplyr::select(-issue_url) %>%
          kableExtra::kable(escape = FALSE) %>%
          kableExtra::kable_styling("striped", "condensed")
      }
      return(tickets_kable)
    }

    output$team_tickets <- function() {
      if (class(tickets) != "data.frame") {
        tickets_kable <- "Credenciales inválidas. Por favor use el gadget de autentificación."
      } else if (nrow(tickets) == 0) {
        tickets_kable <- "No se encontraron tiquetes en el repositorio."
      }  else {
        # Select tickets by open status
        tickets <- tickets %>%
          dplyr::select(assignee.login, number, title, due_date, url) %>%
          dplyr::mutate(assignee.login = ifelse(is.na(assignee.login), "-",
                                         assignee.login)) %>%
          dplyr::filter(assignee.login != credenciales$usuario) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"),
                          sep = "T") %>%
          dplyr::select(-hour) %>%
          dplyr::mutate(due_date = lubridate::ymd(due_date) -
                          lubridate::today()) %>%
          dplyr::mutate(due_date = as.numeric(due_date)) %>%
          tidyr::separate(col = url,
                   into = c("borrar", "issue_url"), sep = "repos/") %>%
          dplyr::select(-borrar) %>%
          dplyr::mutate(issue_url = paste(base_url = paste0("https://", credenciales$url),
               issue_url, sep = "/")) %>%
          dplyr::arrange(desc(due_date)) %>%
          dplyr::rename(tickets, Title = title, Nr = numbe, Due = due_date,
                 User = assignee.login)

        suppressWarnings(verdes <- RColorBrewer::brewer.pal(nrow(tickets), "Greens"))
        suppressWarnings(rojos <- RColorBrewer::brewer.pal(nrow(tickets), "Reds"))

        tickets_kable <- tickets %>%
          dplyr::mutate(
            Due = ifelse(Due < 0,
                         kableExtra::cell_spec(Due, color = "white",
                                   bold = TRUE, background = rojos),
                         kableExtra::cell_spec(Due, color = "white",
                                   bold = TRUE, background = verdes)),
            Nr = kableExtra::text_spec(Nr, link = issue_url),
            Due = ifelse(is.na(Due), "-", Due)) %>%
          dplyr::select(-issue_url) %>%
          kableExtra::kable(escape = FALSE) %>%
          kableExtra::kable_styling("striped", "condensed")
      }
      return(tickets_kable)

    }

    output$quick_links <- function()  {
      if (class(tickets) != "data.frame") {
        quick_links <- "Credenciales inválidas. Por favor use el gadget de autentificación."
      } else {
      # Get closed tickets link
      close_tickets_url <- "issues?q=&type=all&sort=&state=closed&labels=0&milestone=0&assignee=0"
      ixurl <- paste0("https://", credenciales$url)
      close_tickets_url <- paste(ixurl, owner, repository,
            close_tickets_url, sep = "/")

      # Get milestones link
      milestones_url <- paste(ixurl, owner, repository, "milestones", sep = "/")

      # Get Wiki link
      wiki_url <- paste(ixurl, owner, repository, "wiki", sep = "/")

      # Get project link
      project_url <- paste(ixurl, owner, sep = "/")

      # Final table
      links <- c(close_tickets_url, milestones_url, wiki_url, project_url)
      URL <- c("Closed tickets", "Milestones", "Wiki", "Project")
      quick_links <- data.frame(links, URL)

      # Table with kableExtra
      quick_links <- quick_links %>%
        dplyr::mutate(
          URL = kableExtra::text_spec(URL, link = links)) %>%
        dplyr::select(-links) %>%
        kableExtra::kable(escape = FALSE, align = "c") %>%
        kableExtra::kable_styling("striped", "condensed", position = "center",
                      font_size = 20)
      quick_links <- gsub("<thead>.*</thead>", "", quick_links)
      }
      return(quick_links)
    }

    observeEvent(input$done, {
      stopApp(TRUE)
    })

    }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}



