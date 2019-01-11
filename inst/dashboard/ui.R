# ixplorer ui -----------------------------------------------------------------

ui <- navbarPage("ixplorer",
  id = "tabs",
  useShinyjs(),
  extendShinyjs(text = "jscode", functions = c("closeWindow")),
  actionButton("close_app", label = "Close ixplorer Dashboard",
               style = "color: #fff; background-color: #2B7C8D")
  )
