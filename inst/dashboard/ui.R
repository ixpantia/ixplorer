# Header ----------------------------------------------------------------------

ui <- navbarPage(
  "Navbar page",
  id = "tabs",
  tabPanel("Home",
           h2("home")),
  uiOutput("new_sidebar")

)

