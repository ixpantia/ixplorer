sidebar_elements_UI <- function(id) {

  ns <- NS(id)

  sidebarMenu(
    sidebarMenuOutput(ns("proyecto"))#,
#    sidebarMenuOutput(ns("repositorio"))
   )
}


sidebar_elements <- function(input, output, session,
                             proyecto = "p-defecto",
                             repositorios = c("r-defecto")) {

  output$proyecto <- renderMenu({
    sidebarMenu(
        menuItem(proyecto, icon = icon("th"), tabName = "widgets",
          lapply(repositorios, function(i) {
             menuSubItem(i, tabName = "dashboard", icon = icon("dashboard"))
          })
        )
    )
  })

}
