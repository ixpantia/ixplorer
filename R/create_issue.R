#' @import shiny
#' @import miniUI
#' @import gitear
NULL

#' Create issue
#'
#' @export
create_issue <- function() {

  ui <- miniPage(
    gadgetTitleBar("Create a new issue",
                   left = miniTitleBarCancelButton(inputId = "cancel",
                                                   label = "Cancel",
                                                   primary = FALSE),
                   right = miniTitleBarButton(inputId = "done",
                                              label = "Done",
                                              primary = TRUE)),

    miniContentPanel(
      textInput(inputId = "issue_title",
                label = "Issue title",
                width = "100%",
                placeholder = "Brief description of your issue"),

      textAreaInput(inputId = "issue_description",
                    label = "Description",
                    width = "190%",
                    resize = "vertical",
                    rows = 10,
                    placeholder = "Describe the issue you have encountered")
    )
  )

  server <- function(input, output, session) {

    # Leer ixplorer y poner condicionales -------------------------
    if(file.exists(".ixplorer")){
      gitear_access <- readLines(".ixplorer")
      return(gitear_access)
    } else {
      print("There is no authentication yet, please use the Authentication gadget")
    }

    # IXTOKEN ------
    if(str_detect(gitear_access, "IXTOKEN")){
      # separar el token de ixtoken asegurandonos que entre el ixtoken
      intake <- str_subset(gitear_access, "IXTOKEN") %>%
        str_split("=", simplify = TRUE)
      intake <- intake[1,2]
      # tomar ese elemento y unirlo en el Sys.setenv
      Sys.setenv("IXTOKEN" = intake)
      # Dar mensaje de error si no hay intake
    } else {
      print("There is no ixplorer token, please use  the Authentication gadget")
    }

    # IXURL ------
    if(str_detect(gitear_access, "IXURL")){
      # separar el url de ixtoken asegurandonos que entre el ixtoken
      intake <- str_subset(gitear_access, "IXURL") %>%
        str_split("=", simplify = TRUE)
      intake <- intake[1,2]
      # tomar ese elemento y unirlo en el Sys.setenv
      Sys.setenv("IXURL" = intake)
      # Dar mensaje de error si no hay intake
    } else {
      print("There is no ixplorer URL, please use  the Authentication gadget")
    }

    # IXOWNER ------
    if(str_detect(gitear_access, "IXOWNER")){
      # separar el repo de ixrepo asegurandonos que entre el ixrepo
      intake <- str_subset(gitear_access, "IXOWNER") %>%
        str_split("=", simplify = TRUE)
      intake <- intake[1,2]
      # tomar ese elemento y unirlo en el Sys.setenv
      Sys.setenv("IXOWNER" = intake)
      # Dar mensaje de error si no hay intake
    } else {
      print("There is no ixplorer project name, please use  the Authentication gadget")
    }

    # IXUREPO ------
    if(str_detect(gitear_access, "IXREPO")){
      # separar el repo de ixrepo asegurandonos que entre el ixrepo
      intake <- str_subset(gitear_access, "IXREPO") %>%
        str_split("=", simplify = TRUE)
      intake <- intake[1,2]
      # tomar ese elemento y unirlo en el Sys.setenv
      Sys.setenv("IXREPO" = intake)
      # Dar mensaje de error si no hay intake
    } else {
      print("There is no ixplorer repository name, please use  the Authentication gadget")
    }

    # IXUSER ------
    if(str_detect(gitear_access, "IXUSER")){
      # separar el repo de ixrepo asegurandonos que entre el ixrepo
      intake <- str_subset(gitear_access, "IXUSER") %>%
        str_split("=", simplify = TRUE)
      intake <- intake[1,2]
      # tomar ese elemento y unirlo en el Sys.setenv
      Sys.setenv("IXUSER" = intake)
      # Dar mensaje de error si no hay intake
    } else {
      print("There is no ixplorer user name, please use  the Authentication gadget")
    }

    # ----------------------------------------------------------------

    observeEvent(input$done, {
      gitear::create_issue(base_url = Sys.getenv("IXURL"),
                           api_key = Sys.getenv("IXTOKEN"),
                           owner = Sys.getenv("IXOWNER"),
                           repo = Sys.getenv("IXREPO"),
                           title = input$issue_title,
                           body =  input$issue_description)
      stopApp(NULL)
    })

    observeEvent(input$cancel, {
      # do nothing
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("ixplorer"))
}
