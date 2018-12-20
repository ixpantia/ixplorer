sidebar_elements_UI <- function(id) {

  ns <- NS(id)

  sidebarMenu(
    sidebarMenuOutput(ns("proyecto"))
   )
}


sidebar_elements <- function(input, output, session,
                             proyecto = "No hay proyectos",
                             repositorios = c("No hay repositorios")) {

  output$proyecto <- renderMenu({
    sidebarMenu(
        menuItem(proyecto, icon = icon("th"), tabName = make.names(proyecto),
          lapply(repositorios, function(i) {
             menuSubItem(i, tabName = make.names(repositorios), icon = icon("dashboard"))
          })
        )
    )
  })

}
