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
sidebar <- dashboardSidebar(collapsed = FALSE,
  sidebarMenu(
    sidebar_elements_UI("sidebar")
  )
)


# Content ---------------------------------------------------------------------
body <- dashboardBody(includeCSS("www/css/coocique.css"),
          tabItems(
             bodyelementsUI("body")
            )
          )

## App completo ----------------------------------------------------------------
dashboardPage(
  skin = "orange",
  header,
  sidebar,
  body
)

