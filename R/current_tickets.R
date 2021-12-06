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
current_tickets <- function() {

  # if (repository == "current") { ##NOTAR
  #   repository <- basename(rstudioapi::getActiveProject())
  # }

  # Code for using keyring (To be implemented later)---------------------------
  # credentials <- tryCatch(
  #   keyring::key_get(paste0("token_", instance)),
  #   error = function(cond) "no_credentials")

  # credentials <- tryCatch({
  #   keyrings <- keyring::keyring_list()
  #   error = function(cond) "no_credentials"})) #Needs checking
  #
  # if (credentials$persistence == FALSE) {
  #   keyring::key_delete(paste0("token_", instance))
  # }

  # Get instance --------------------------------------------------------------

  instance = Sys.getenv("ixplorer_instance") # variable to check gadget workflow

  credentials <- keyring::keyring_list() # while tryCatch() is checked

  # Get keys -------------------------------------------------------------------
  authentication_user_name <- key_get("ixplorer_user_name", keyring = instance)
  authentication_owner <- key_get("ixplorer_project", keyring = instance)
  authentication_repo <- key_get("ixplorer_repo", keyring = instance)
  authentication_repository <- key_get("ixplorer_repo", keyring = instance)
  authentication_base_url <- key_get("ixplorer_url", keyring = instance)

  # UI -------------------------------------------------------------------------
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
  # sever ---------------------------------------------------------------------
  server <- function(input, output, session) {

    output$warning <- renderText({   ##NO NEED
      msg <- ifelse(credentials == "no_credentials",
                    "No credential file available", "")
      return(msg)
    })

    # output$warning <- renderText({
    #   msg <- if (credentials == "no credentials"){
    #     "No credential file available"
    #   }
    #   return(msg)
    # })

    # Get tickets and configurate credentials
    tickets <- tryCatch({

      # gitear::get_issues_open_state(  old credential handling
      #   base_url = credentials$url,
      #   api_key = credentials$token,
      #   owner = credentials$owner,
      #   repo = repository)

      gitear::get_issues_open_state(
        base_url = key_get("ixplorer_url", keyring = instance),
        api_key = key_get("ixplorer_token", keyring = instance),
        owner = key_get("ixplorer_project", keyring = instance),
        repo = key_get("ixplorer_repo", keyring = instance))

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
          dplyr::filter(assignee.login == authentication_user_name) %>%
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
            issue_url = paste(base_url = paste0("https://", authentication_base_url),
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
          dplyr::filter(assignee.login != authentication_user_name) %>% ##CHANGED TO KEY_GET
          tidyr::separate(col = due_date, into = c("due_date", "hour"),
                          sep = "T") %>%
          dplyr::select(-hour) %>%
          dplyr::mutate(due_date = lubridate::ymd(due_date) -
                          lubridate::today()) %>%
          dplyr::mutate(due_date = as.numeric(due_date)) %>%
          tidyr::separate(col = url,
                          into = c("borrar", "issue_url"), sep = "repos/") %>%
          dplyr::select(-borrar) %>%
          dplyr::mutate(issue_url = paste(base_url = paste0("https://", authentication_base_url),
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
        #ixurl <- paste0("https://", authentication_base_url)
        ixurl <- authentication_base_url
        close_tickets_url <- paste(ixurl, authentication_owner, authentication_repository,
                                   close_tickets_url, sep = "/")

        # Get milestones link
        milestones_url <- paste(ixurl, authentication_owner, authentication_repository, "milestones", sep = "/")

        # Get Wiki link
        wiki_url <- paste(ixurl, authentication_owner, authentication_repository, "wiki", sep = "/")

        # Get project link
        project_url <- paste(ixurl, authentication_owner, sep = "/")

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

#' @title Tiquetes actuales
#' @description Vea tiquetes para un usuario específico en una computadora y
#' obtenga enlaces a su ixplorer según las credenciales utilizadas en el gadget
#' de autenticación.
#'
#' @param instancia instancia de ixplorer (Ej: "secure", "masterclass", "prueba")
#' @param propietario el nombre del proyecto donde el repositorio está ubicado en ixplorer
#' @param repositorio nombre del repositorio donde están los tiquetes
#'
#' @export

tiquetes_actuales <- function(...) {
  current_tickets()
}

