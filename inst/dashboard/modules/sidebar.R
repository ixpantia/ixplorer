sidebar_elements_UI <- function(id) {

  ns <- NS(id)

  sidebarMenu(
    sidebarMenuOutput(ns("proyecto"))#,
#    sidebarMenuOutput(ns("repositorio"))
   )
}


sidebar_elements <- function(input, output, session,
                             proyecto = "p-defecto") {

  output$proyecto <- renderMenu({
    sidebarMenu(
        menuItem(proyecto, icon = icon("th"), tabName = "widgets",
          menuSubItem("Repositorio", tabName = "dashboard", icon = icon("dashboard"))
        )
    )
  })

}
