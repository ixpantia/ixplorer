#' @import shiny
#' @import miniUI
NULL

#' @title Current tickets
#' @description View tickets for a specific user on a computer and get links to
#'  your ixplorer based on the credentials used in the authentication gadget.
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#' @param owner the name of the project where the repository is located in ixplorer
#' @param repository the name of the repository where the tickets are
#'
#' @export
current_tickets <- function(repository = "current") {

  if (repository == "current") {
    repository <- basename(rstudioapi::getActiveProject())
  }

  # Read credentials from .ixplorer TEMPORAL
  ixplorer_url <- Sys.getenv("IXURL")

  credentials <- tibble::tribble(
    ~url, ~token, ~user, ~owner,
    Sys.getenv("IXURL"), Sys.getenv("IXTOKEN"), Sys.getenv("IXUSER"), Sys.getenv("IXPROJECT")
  )

  instance <- sub("\\..*", "", ixplorer_url)

  # Code for using keyring (To be implemented later)
  # credentials <- tryCatch(
  #   keyring::key_get(paste0("token_", instance)),
  #   error = function(cond) "no_credentials")


  # if (credentials != "no_credentials") {
  #   credentials <- credentials %>%
  #     stringr::str_split("/", simplify = TRUE) %>%
  #     tibble::as_tibble() %>%
  #     dplyr::select(-V1, -V2, -V4) %>%
  #     magrittr::set_names(c("url", "token",
  #                           "user", "owner",
  #                           "persistence")) %>%
  #     # dplyr::mutate(persistence = as.numeric(persistence)) %>%
  #     dplyr::mutate(persistence = as.logical(persistence))
  #
  # }

  # if (credentials$persistence == FALSE) {
  #   keyring::key_delete(paste0("token_", instance))
  # }

  ui <- miniPage(
    miniTitleBar("Current tickets",
                 right = miniTitleBarCancelButton(inputId = "done",
                                                 label = "Done.",
                                                 primary = TRUE)
                 ),
    verbatimTextOutput("warning", placeholder = FALSE),
    miniTabstripPanel(
      miniTabPanel("My tickets", icon = icon("user"),
                   miniContentPanel(
                     tableOutput("my_tickets")
                   )
      ),
      miniTabPanel("Team tickets", icon = icon("users"),
                   miniContentPanel(
                     tableOutput("team_tickets")
                   )
      ),
      miniTabPanel("Links", icon = icon("link"),
                   miniContentPanel(
                     tableOutput("quick_links")
                   )
      )
    )
  )

  server <- function(input, output, session){

    output$warning <- renderText({
      msg <- ifelse(credentials == "no_credentials",
                     "No credential file available", "")
      return(msg)
    })

    # Get tickets and configurate credentials
    tickets <- tryCatch({

      gitear::get_issues_open_state(
           base_url = credentials$url,
           api_key = credentials$token,
           owner = credentials$owner,
           repo = repository)
      # Eliminate this check code
      # check <- gitear::get_issues_open_state (base_url = "https://secure.ixpantia.com",
      #                                         api_key = ixplorer_token,
      #                                         owner = "ixplorer",
      #                                         repo = "ixplorer.es")

      },
      error = function(cond) {
        tickets <- "Invalid"
      })



    output$my_tickets <- function() {
      if (class(tickets) != "data.frame") {
        tickets_kable <- "Invalid credentials. Please use the authentication gadget."
      } else if (nrow(tickets) == 0) {
        tickets_kable <- "No tickets were found in the repository."
      } else {
        # Select tickets by user and tickets link creation
        tickets <- tickets %>%
          dplyr::filter(assignee.login == credentials$user) %>%
          dplyr::select(number, title, due_date, url) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"),
                          sep = "T") %>%
          dplyr::select(-hour) %>%
          dplyr::mutate(due_date = lubridate::ymd(due_date) -
                          lubridate::today()) %>%
          tidyr::separate(col = url,
                   into = c("borrar", "issue_url"), sep = "repos/") %>%
          dplyr::select(-borrar) %>%
          dplyr::mutate(
            issue_url = paste(base_url = paste0("https://", credentials$url),
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
        tickets_kable <- "Invalid credentials. Please use the authentication gadget."
      } else if (nrow(tickets) == 0) {
        tickets_kable <- "No tickets were found in the repository."
      }  else {
        # Select tickets by open status
        tickets <- tickets %>%
          dplyr::select(assignee.login, number, title, due_date, url) %>%
          dplyr::mutate(assignee.login = ifelse(is.na(assignee.login), "-",
                                         assignee.login)) %>%
          dplyr::filter(assignee.login != credentials$user) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"),
                          sep = "T") %>%
          dplyr::select(-hour) %>%
          dplyr::mutate(due_date = lubridate::ymd(due_date) -
                          lubridate::today()) %>%
          dplyr::mutate(due_date = as.numeric(due_date)) %>%
          tidyr::separate(col = url,
                   into = c("borrar", "issue_url"), sep = "repos/") %>%
          dplyr::select(-borrar) %>%
          dplyr::mutate(issue_url = paste(base_url = paste0("https://", credentials$url),
               issue_url, sep = "/")) %>%
          dplyr::arrange(desc(due_date)) %>%
          dplyr::rename(Title = title, Nr = number, Due = due_date,
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
        quick_links <- "Invalid credentials. Please use the authentication gadget."
      } else {
      # Get closed tickets link
      close_tickets_url <- "issues?q=&type=all&sort=&state=closed&labels=0&milestone=0&assignee=0"
      ixurl <- paste0("https://", credentials$url)
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



