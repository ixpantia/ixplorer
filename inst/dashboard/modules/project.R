project_body_UI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(6, plotOutput(ns("plot1"))),
    column(6, plotOutput(ns("plot2")))
  )

}

project_body <- function(input, output, session,
                         project_data = "No hay proyectos") {

  output$plot1 <- renderPlot({
    plot(project_data)
  })

  output$plot2 <- renderPlot({
   plot(project_data)
  })
}

