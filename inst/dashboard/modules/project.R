project_body_UI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("project_tabitem"))

}

project_body <- function(input, output, session,
                        project_name) {

  tabname <- make.names(project_name)

  output$project_tabitem <- renderUI({
    tabItem(tabname = "widgets",
      h2("hello, hello")
      )
  })

}
