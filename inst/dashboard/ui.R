# ixplorer ui -----------------------------------------------------------------

ui <- navbarPage("ixplorer",
  id = "tabs",
  actionButton("close_app", label = "Close ixplorer Dashboard",
               style = "color: #fff; background-color: #337ab7")
  )
