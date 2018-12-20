# Header ----------------------------------------------------------------------
header <- dashboardHeader(
  title = "ixplorer",
  tags$li(a(href = "http://www.ixpantia.com",
            img(src = "img/ixpantia.png",
                title = "ixpantia", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
)

# Menu a la izquierda----------------------------------------------------------
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  uiOutput("new_sidebar")
)


# Content ---------------------------------------------------------------------
body <- dashboardBody(
  includeCSS("www/css/ixplorer.css"),
  tabItems(
    uiOutput("new_body")
  )
#   tabItems(
#    lapply(bodies, function(body) {
#        project_body_UI(body)
#    })
#   )
)

## App completo ----------------------------------------------------------------
dashboardPage(
  skin = "yellow",
  header,
  sidebar,
  body
)

