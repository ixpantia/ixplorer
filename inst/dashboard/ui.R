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
   uiOutput("new_sidebar")
#   sidebar_elements_UI("ix_interface_1"),
#   sidebar_elements_UI("ix_interface_2")
)


# Content ---------------------------------------------------------------------
body <- dashboardBody(includeCSS("www/css/ixplorer.css")#,
#          tabItems(
#             body_elements_UI("body")
#            )
          )

## App completo ----------------------------------------------------------------
dashboardPage(
  skin = "yellow",
  header,
  sidebar,
  body
)

