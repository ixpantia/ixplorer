repository_UI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, plotOutput(ns("plot1"))),
    column(6, plotOutput(ns("plot2")))
  )
}

repository <- function(input, output, session,
                         repo_data = "No Repositories") {

  output$plot1 <- renderPlot({
    plot(repo_data)
  })

  output$plot2 <- renderPlot({
   plot(repo_data)
  })

}

