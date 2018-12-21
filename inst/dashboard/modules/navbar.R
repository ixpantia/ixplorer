library(shiny)
library(ggplot2)

lista_proyectos <- list()
lista_proyectos$lagarita <- c("Diego", "Bellota")
lista_proyectos$sancarlos <- c("hola", "hello", "goodbye")

# modules ---------
scatterPlot <- function(datos) {
  ggplot(data = data, aes_string(x = data[, 1], y = data[,2])) +
    geom_point(aes(color = selected_)) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}

linkedScatterUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, plotOutput(ns("plot1"))),
    column(6, plotOutput(ns("plot2")))
  )
}

linkedScatter <- function(input, output, session,
                          datos) {
  output$plot1 <- renderPlot({
    plot(datos)
  })

  output$plot2 <- renderPlot({
   plot(datos)
  })

}

# ui ------------------------------------

ui <- navbarPage("ixplorer", id = "tabs")

server <- function(input, output, session) {

  proyectos <- names(lista_proyectos)

  observe({
    lapply(proyectos, function(proyecto) {
      appendTab(inputId = "tabs",
        tabPanel(proyecto,
          tabsetPanel(id = proyecto)
         ))
      })
   })

  observe({
    for (proyecto in proyectos) {
      lapply(lista_proyectos[proyecto][[1]], function(repo) {
        appendTab(inputId = proyecto,
                  tabPanel(repo,
                           h2(paste("encabezado de ", repo)),
                           linkedScatterUI(repo)
                           ))
      })
   }
   })

  for (proyecto in proyectos) {
    lapply(lista_proyectos[proyecto][[1]], function(repo) {
       callModule(linkedScatter, repo, datos = mtcars)
    })
  }


}
shinyApp(ui, server)
