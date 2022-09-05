#' @import shiny
#' @import miniUI
#' @import shiny.i18n
#' @import shinyWidgets
NULL

#' @title Current tickets
#' @description View tickets for a specific user on a computer and get links to
#'  your ixplorer based on the credentials used in the authentication gadget.
#'
#' @param instance ixplorer instance (Eg: "secure", "masterclass", "prueba")
#' @return No return value, called for side effects
#' @export
current_tickets <- function(instance = "saved") {

  # Look for instance ---------------------------------------------------------

if (instance == "saved") {

    current_instance <- get_instance()

    if (current_instance != "none") {

      saved_instances <- keyring::keyring_list() %>%
        filter(keyring == current_instance) %>%
        rename(instance = keyring)
      instance <- toString(saved_instances[1])


    } else {

      stop("There are no saved instances")

    }

  } else {

    ix_instance <- paste0("ixplorer_", instance)

    saved_instances <- keyring::keyring_list() %>%
      filter(keyring == ix_instance) %>%
      rename(instance = keyring)

    if (nrow(saved_instances) == 0) {

      stop("There is no instance named ", instance)
    }
  }

  # Define translator ---------------------------------------------------------

  i18n <- shiny.i18n::Translator$new(
    translation_json_path = "https://storage.googleapis.com/ixplorer/translation.json"
    )

  # Set translation language --------------------------------------------------

  language <- keyring::key_get("ixplorer_language", keyring = instance)
  i18n$set_translation_language(language)


  credentials <- keyring::keyring_list() # while tryCatch() is checked

  # Get keys -------------------------------------------------------------------
  authentication_user_name <- keyring::key_get("ixplorer_user_name",
                                               keyring = instance)
  authentication_owner <- keyring::key_get("ixplorer_project",
                                           keyring = instance)
  authentication_repo <- keyring::key_get("ixplorer_repo",
                                          keyring = instance)
  authentication_repository <- keyring::key_get("ixplorer_repo",
                                                keyring = instance)
  authentication_base_url <- keyring::key_get("ixplorer_url",
                                              keyring = instance)

  # UI -------------------------------------------------------------------------
  ui <- miniPage(
    miniTitleBar(i18n$t("Current tickets"),
                 right = miniTitleBarCancelButton(inputId = "done",
                                                  label = i18n$t("Done."),
                                                  primary = TRUE)
    ),
    verbatimTextOutput(i18n$t("warning"), placeholder = FALSE),
    miniTabstripPanel(
      miniTabPanel(i18n$t("My tickets"), icon = icon("user"),
                   miniContentPanel(
                     tableOutput("my_tickets")
                   )
      ),
      miniTabPanel(i18n$t("Team tickets"), icon = icon("users"),
                   miniContentPanel(
                     tableOutput("team_tickets")
                   )
      ),
      miniTabPanel(i18n$t("Links"), icon = icon("link"),
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


    # Get tickets and configurate credentials
    tickets <- tryCatch({

      gitear::get_issues_open_state(
        base_url = keyring::key_get("ixplorer_url", keyring = instance),
        api_key = keyring::key_get("ixplorer_token", keyring = instance),
        owner = keyring::key_get("ixplorer_project", keyring = instance),
        repo = keyring::key_get("ixplorer_repo", keyring = instance))

    },
    error = function(cond) {
      tickets <- "Invalid"
    })


    output$my_tickets <- function() {
      if (methods::is(tickets)[1] != "data.frame") {
        tickets_kable <- "Invalid credentials.
        Please use the authentication gadget."
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
            issue_url = paste(base_url = paste0(
                              "https://",
                              authentication_base_url),
                              issue_url,
                              sep = "/")) %>%
          dplyr::arrange(desc(due_date)) %>%
          dplyr::rename(Title = title, Nr = number, Due = due_date)

        suppressWarnings(
            verdes <- RColorBrewer::brewer.pal(nrow(tickets), "Greens"))
        suppressWarnings(
            rojos <- RColorBrewer::brewer.pal(nrow(tickets), "Reds"))

        tickets_kable <- tickets %>%
          dplyr::mutate(Due = ifelse(Due < 0,
                               kableExtra::cell_spec(Due,
                                            color = "white",
                                            bold = TRUE,
                                            background = rojos),
                               kableExtra::cell_spec(Due,
                                            color = "white",
                                            bold = TRUE,
                                            background = verdes)),
                        Nr = kableExtra::text_spec(Nr, link = issue_url),
                        Due = ifelse(is.na(Due), "-", Due)) %>%
          dplyr::select(-issue_url) %>%
          kableExtra::kable(escape = FALSE) %>%
          kableExtra::kable_styling("striped", "condensed")
      }
      return(tickets_kable)
    }

    output$team_tickets <- function() {
      if (methods::is(tickets)[1] != "data.frame") {
        tickets_kable <- "Invalid credentials.
        Please use the authentication gadget."
      } else if (nrow(tickets) == 0) {
        tickets_kable <- "No tickets were found in the repository."
      }  else {
        # Select tickets by open status
        tickets <- tickets %>%
          dplyr::select(assignee.login, number, title, due_date, url) %>%
          dplyr::mutate(assignee.login = ifelse(is.na(assignee.login), "-",
                                                assignee.login)) %>%
          dplyr::filter(assignee.login != authentication_user_name) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"),
                          sep = "T") %>%
          dplyr::select(-hour) %>%
          dplyr::mutate(due_date = lubridate::ymd(due_date) -
                          lubridate::today()) %>%
          dplyr::mutate(due_date = as.numeric(due_date)) %>%
          tidyr::separate(col = url,
                          into = c("borrar", "issue_url"), sep = "repos/") %>%
          dplyr::select(-borrar) %>%
          dplyr::mutate(issue_url = paste(base_url = paste0(
                                      "https://", authentication_base_url),
                                      issue_url, sep = "/")) %>%
          dplyr::arrange(desc(due_date)) %>%
          dplyr::rename(Title = title, Nr = number, Due = due_date,
                        User = assignee.login)

        suppressWarnings(
          verdes <- RColorBrewer::brewer.pal(nrow(tickets), "Greens"))
        suppressWarnings(
          rojos <- RColorBrewer::brewer.pal(nrow(tickets), "Reds"))

        tickets_kable <- tickets %>%
          dplyr::mutate(
            Due = ifelse(Due < 0,
                         kableExtra::cell_spec(Due,
                                      color = "white",
                                      bold = TRUE, background = rojos),
                         kableExtra::cell_spec(Due,
                                      color = "white",
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
      if (methods::is(tickets)[1] != "data.frame") {
        quick_links <- "Invalid credentials. 
        Please use the authentication gadget."
      } else {
        # Get closed tickets link
        close_tickets_url <- "issues?q=&type=all&sort=&state=closed&labels=0&milestone=0&assignee=0"
        ixurl <- authentication_base_url
        close_tickets_url <- paste(ixurl, authentication_owner,
                                   authentication_repository,
                                   close_tickets_url, sep = "/")

        # Get milestones link
        milestones_url <- paste(ixurl, 
                                authentication_owner, 
                                authentication_repository, 
                                "milestones", sep = "/")

        # Get Wiki link
        wiki_url <- paste(ixurl, 
                          authentication_owner, 
                          authentication_repository, 
                          "wiki", sep = "/")

        # Get project link
        project_url <- paste(ixurl, authentication_owner, sep = "/")

        # Final table
        links <- c(close_tickets_url,
                   milestones_url,
                   wiki_url,
                   project_url)
        URL <- c((i18n$t("Closed tickets")),
                 (i18n$t("Milestones")),
                 "Wiki", (i18n$t("Project")))
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
#' @param instancia instancia de ixplorer (Ej: "secure", "masterclass",
#'        "prueba")
#' @return No hay valor de retorno - se llama por su efecto secundario
#' @export
tiquetes_actuales <- function(instancia = "guardada") {

  if (instancia == "guardada") {

    current_tickets(instance = "saved")

  } else {

    current_tickets(instance = instancia)

  }
}

