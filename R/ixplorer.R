#' @import shiny
#' @import miniUI
#' @import dplyr
#' @import kableExtra
NULL

#' ixplorer reports
#'
#' Visualize the tickets of an specific user, a team and get the quick links to
#' your ixplorer based on the credentials used in gadget authenticate.
#'
#' @export
ix_tickets <- function() {
  ui <- miniPage(
    gadgetTitleBar("ixplorer Reports"),
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
      miniTabPanel("Quick links", icon = icon("link"),
                   miniContentPanel(
                     tableOutput("quick_links")
                   )
      )
    )
  )

  server <- function(input, output, session){

    access_file <- verify_ixplorer_file()
    msg <- if (access_file$empty == TRUE) {
      "no credential file available"
    } else {
      ixplorer:::set_authentication(access_data = access_file$gitear_access)
    }

    output$warning <- renderText({
      msg <- if (access_file$empty == TRUE) {
        "no credential file available"
      } else {
        set_authentication(access_data = access_file$gitear_access)
      }
      return(msg)
    })

    # Get tickets and configurate credentials
    # tickets <- if (access_file$empty == TRUE) {
    #   tickets <- data.frame(character(0))
    #   warning("no access data")
    # } else {
    #   tickets <- try(gitear::get_issues_open_state(base_url = Sys.getenv("IXURL"),
    #                                           api_key = Sys.getenv("IXTOKEN"),
    #                                           owner = Sys.getenv("IXPROJECT"),
    #                                           repo = Sys.getenv("IXREPO")))
    #   if (class(tickets) == "try-error") {
    #     print("Invalid credentials")
    #   } else {
    #     ixplorer_user = Sys.getenv("IXUSER")
    #     # Untie table
    #     tickets <- jsonlite::flatten(tickets)
    #   }
    #
    # }

    tickets <- if (access_file$empty == TRUE) {
        tickets <- data.frame(character(0))
        warning("no access data")
      } else {
        tickets <- tryCatch(
          {
            gitear::get_issues_open_state(base_url =
                                            Sys.getenv("IXURL"),
                                          api_key = Sys.getenv("IXTOKEN"),
                                          owner = Sys.getenv("IXPROJECT"),
                                          repo = Sys.getenv("IXREPO"))


            ixplorer_user = Sys.getenv("IXUSER")

            # Untie table
            tickets <- jsonlite::flatten(tickets)
          },
          error = function(cond) {
            tickets <- dplyr::tibble()
          }
        )


      }


    output$my_tickets <- function() {
      if (nrow(tickets) == 0) {
        tickets_kable <- "No tickets found in repository"
      } else {
        # Select tickets by user and tickets link creation
        tickets <- tickets %>%
          filter(assignee.login == ixplorer_user) %>%
          select(number, title, due_date, url) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
          select(-hour) %>%
          mutate(due_date = lubridate::ymd(due_date) - lubridate::today()) %>%
          tidyr::separate(col = url,
                   into = c("borrar", "issue_url"), sep = "repos/") %>%
          select(-borrar) %>%
          mutate(issue_url = paste(Sys.getenv("IXURL"), issue_url, sep = "/")) %>%
          arrange(desc(due_date))

        tickets <- rename(tickets, Title = title)
        tickets <- rename(tickets, Nr = number)
        tickets <- rename(tickets, Due = due_date)

        verdes <- RColorBrewer::brewer.pal(nrow(tickets), "Greens")
        rojos <- RColorBrewer::brewer.pal(nrow(tickets), "Reds")

        tickets_kable <- tickets %>%
          mutate(Due = ifelse(Due < 0, cell_spec(Due, color = "white",
                                                 bold = TRUE, background = rojos),
                              cell_spec(Due, color = "white",
                                        bold = TRUE, background = verdes)),
                 Nr = text_spec(Nr, link = issue_url)) %>%
          select(-issue_url) %>%
          kable(escape = FALSE) %>%
          kable_styling("striped", "condensed")
      }
      return(tickets_kable)
    }

    output$team_tickets <- function(){
      if (nrow(tickets) == 0) {
        tickets_kable <- "No tickets found in repository"
      }  else {
        # Select tickets by open status
        tickets <- tickets %>%
          select(assignee.login, number, title, due_date, url) %>%
          mutate(assignee.login = ifelse(is.na(assignee.login), "-",
                                         assignee.login)) %>%
          filter(assignee.login != ixplorer_user) %>%
          tidyr::separate(col = due_date, into = c("due_date", "hour"), sep = "T") %>%
          select(-hour) %>%
          mutate(due_date = lubridate::ymd(due_date) - lubridate::today()) %>%
          mutate(due_date = as.numeric(due_date)) %>%
          tidyr::separate(col = url,
                   into = c("borrar", "issue_url"), sep = "repos/") %>%
          select(-borrar) %>%
          mutate(issue_url = paste(Sys.getenv("IXURL"), issue_url, sep = "/")) %>%
          arrange(desc(due_date))

        tickets <- rename(tickets, Title = title)
        tickets <- rename(tickets, Nr = number)
        tickets <- rename(tickets, Due = due_date)
        tickets <- rename(tickets,  User = assignee.login)

        verdes <- RColorBrewer::brewer.pal(nrow(tickets), "Greens")
        rojos <- RColorBrewer::brewer.pal(nrow(tickets), "Reds")

        tickets_kable <- tickets %>%
          mutate(
            Due = ifelse(Due < 0,
                         cell_spec(Due, color = "white",
                                   bold = TRUE, background = rojos),
                         cell_spec(Due, color = "white",
                                   bold = TRUE, background = verdes)),
            Nr = text_spec(Nr, link = issue_url)) %>%
          select(-issue_url) %>%
          kable(escape = FALSE) %>%
          kable_styling("striped", "condensed")
      }
      return(tickets_kable)

    }

    output$quick_links <- function(){
      # Get closed tickets link
      close_tickets_url <- "issues?q=&type=all&sort=&state=closed&labels=0&milestone=0&assignee=0"
      ixurl <- sub("/$", "", Sys.getenv("IXURL"))
      close_tickets_url <- paste(ixurl, Sys.getenv("IXPROJECT"), Sys.getenv("IXREPO"),
            close_tickets_url, sep = "/")

      # Get milestones link
      milestones_url <- paste(ixurl, Sys.getenv("IXPROJECT"),
                              Sys.getenv("IXREPO"), "milestones", sep = "/")

      # Get Wiki link
      wiki_url <- paste(ixurl, Sys.getenv("IXPROJECT"),
                        Sys.getenv("IXREPO"), "wiki", sep = "/")

      # Get project link
      project_url <- paste(ixurl, Sys.getenv("IXPROJECT"), sep = "/")

      # Final table
      links <- c(close_tickets_url, milestones_url, wiki_url, project_url)
      URL <- c("Clossed tickets", "Milestones", "Wiki", "Project")
      quick_links <- data_frame(links, URL)

      # Table with kableExtra
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



