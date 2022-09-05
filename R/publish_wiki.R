#' Publish an rmarkdown into wiki repo
#'
#' @title Publish into wiki repo
#' @description Publish an specific rmarkdown or qmd into wiki repository, specifying
#'   the path of the wiki repository once that you already clone this repository
#'   into your machine. See Details if is your first time publishing in a wiki
#'   repository.
#'
#' @details If this is the first time you are going to publish in a wiki, you
#'   must follow these steps:
#'
#' 1. The wiki repository must be cloned to a local working folder. To do this
#' first go to the wiki tab of your repository and you will need to manually
#' create the first wiki page. If you don't do this it will tell you that the
#' repository does not exist.
#'
#' 2. Once you have created the wiki home page, you will need to clone the wiki
#' repository just like you would any other repository on your computer (using
#' the link that is made available to you once you create the wiki home page).
#'
#' 3. Once you have cloned the wiki on your computer, what you should indicate
#' in the `publica_wiki` function is the` path_wiki_repo`. This is the full
#' address on your computer where this repository you cloned is located. For
#' example: `/home/client/project/wiki_repo`
#'
#' @param report the path of rmarkdown or qmd file
#' @param path_wiki_repo the complete path of wiki repository.
#' @param automatic_update if you like to do automatically the pull, commit and
#'  push set in TRUE, if you like to do manually, set FALSE. Default is TRUE.
#' @param quiet_render_logs if you like to see the render logs of rmarkdown,
#'  set in TRUE, in otherwise set FALSE. Default is TRUE.
#' @return Publish a md into wiki repository
#' @examples
#'
#' \dontrun{
#' publish_wiki(report = "eda.Rmd",
#'              path_wiki_repo = "/home/cliente/proyecto/wiki_repo")
#' }
#'
#' @export

publish_wiki <- function(report, path_wiki_repo, automatic_update = FALSE,
                         quiet_render_logs = FALSE) {

  # Get the report extension

  extension <- stringr::str_sub(report,start = -3, end = -1)

  if (!(extension %in% c("Rmd","qmd"))) {

    stop("Supported file types are .Rmd or .qmd")
  }



  # Repare path_wiki_repo

  if (stringr::str_sub(path_wiki_repo,
                       start = stringr::str_length(path_wiki_repo)) == "/") {

    path_wiki_repo <- stringr::str_sub(
                        path_wiki_repo,
                        start = 1,
                        end = stringr::str_length(path_wiki_repo) - 1)
  }

  # Make pull if automatic_update is TRUE

  if (automatic_update) {

    if (nrow(gert::git_status(repo = path_wiki_repo)) > 0) {
      gert::git_stash_save(repo = path_wiki_repo)
      message("Some of your changes to the wiki repository were put into stash.
              If you want to retrieve them you must go to the wiki repository
              and run: git stash apply")
    }

    gert::git_pull(repo = path_wiki_repo)
  }

  # Render the rmarkdown to github_document output

  quarto::quarto_render(report, output_format = "gfm",
                        quiet = quiet_render_logs)

  # Copy md to wiki repository

  dot_name <- paste0(".",extension)

  base_name <- stringr::str_remove_all(basename(report), dot_name)


  md <- paste0(base_name, ".md")

  path_md <- paste0(stringr::str_remove_all(report, basename(report)), md)

  file.copy(path_md, path_wiki_repo, overwrite = TRUE)
  file.remove(path_md)

  # Copy images to wiki repository

  files_folder <- paste0(base_name, "_files")

  files_path <- paste0(stringr::str_remove_all(report, basename(report)),
                       files_folder)

  if (dir.exists(files_path)) {

    file.copy(files_path, path_wiki_repo, overwrite = TRUE, recursive = TRUE)
    unlink(files_path, recursive = TRUE, force = TRUE)

  }

  # Update wiki repository with new changes

  if (automatic_update) {

    if (nrow(gert::git_status(repo = path_wiki_repo)) > 0) {

      if (md %in% gert::git_status(repo = path_wiki_repo)$file) {
        gert::git_add(md, repo = path_wiki_repo)
      }

      if (any(stringr::str_detect(gert::git_status(repo = path_wiki_repo)$file,
                                  files_folder))) {

        gert::git_add(files_folder, repo = path_wiki_repo)
      }

      if (any(gert::git_status(repo = path_wiki_repo)$staged)) {

        gert::git_commit(paste0("Actualiza ", base_name), repo = path_wiki_repo)

        gert::git_push(repo = path_wiki_repo)

      } else {

        message(paste0("There are no new changes or something that can be sent in ",
                       base_name, ".", extension))
      }

    } else {

      message(paste0("There are no new changes or something that can be sent in ",
                     base_name, ".", extension))
    }

  } else {

    if (dir.exists(paste0(path_wiki_repo, "/", files_folder))) {

      cat(
        paste("Go to the repository", path_wiki_repo,
              "and execute the following commnads in terminal:"), "\n", "\n",
        paste("git pull"), "\n",
        paste("git add", md), "\n",
        paste("git add", files_folder), "\n",
        paste0("git commit -m 'Update ", base_name, "'"), "\n",
        "git push \n")

    } else {

      cat(
        paste("Go to the repository", path_wiki_repo,
              "and execute the following commnads in terminal:"), "\n", "\n",
        paste("git pull"), "\n",
        paste("git add", md), "\n",
        paste0("git commit -m 'Update ", base_name, "'"), "\n",
        "git push \n")

    }
  }
}


#' Publica un rmarkdown en un wiki repo
#'
#' @title Publica en el wiki repo
#' @description Publique un rmarkdown o qmd específico en el repositorio wiki,
#'   especificando la ruta del repositorio wiki una vez que ya haya clonado este
#'   repositorio en su máquina. Consulte Detalles si es la primera vez que
#'   publica en un repositorio wiki.
#'
#' @details Si es la primera vez que va a publicar en una wiki,  se deben seguir
#'   estos pasos:
#'
#' 1.El repositorio wiki debe clonarse en una carpeta de trabajo local.
#' Para hacer esto, primero vaya a la pestaña wiki de su repositorio
#' y deberá crear manualmente la primera página wiki. Si no lo hace,
#' le dirá que el repositorio no existe.
#'
#' 2. Una vez que haya creado la página de inicio de la wiki,
#' deberá clonar el repositorio de la wiki
#' como lo haría con cualquier otro repositorio en su computadora
#' (usando el enlace que está disponible para usted una vez que haya creado la
#' página de inicio de la wiki).
#'
#' 3. Una vez que haya clonado la wiki en su computadora, lo que debe indicar
#' en la función `publica_wiki` es la` path_wiki_repo`. Esta es la dirección
#' completa en su computadora donde se encuentra este repositorio que clonó.
#' Por ejemplo:  `/home/client/project/wiki_repo`
#'
#' @param reporte la ruta al archivo rmarkdown o qmd
#' @param ruta_repo_wiki la ruta completa al repositorio wiki.
#' @param auto_actualizar Si desea hacer automáticamente el pull, commit y push
#'   establecido en TRUE, si desea hacerlo manualmente, establezca FALSE. El
#'   valor predeterminado es TRUE.
#' @param silenciar_bitacora  Si desea ver la bitacora de renderización
#'   establezca TRUE, de lo contrario establezca FALSE. El valor default es TRUE
#' @return Publica un md en un repositorio wiki.
#' @examples
#'
#' \dontrun{
#' publish_wiki(reporte = "eda.Rmd",
#'              path_wiki_repo = "/home/cliente/proyecto/wiki_repo")
#' }
#'
#' @export


publica_wiki <- function(reporte = report, ruta_repo_wiki = path_wiki_repo,
                         auto_actualizar = TRUE, silenciar_bitacora= FALSE) {


  ## True and False
  if (auto_actualizar == TRUE && silenciar_bitacora == FALSE) {

    publish_wiki(reporte, ruta_repo_wiki,
                 automatic_update = TRUE, quiet_render_logs = FALSE)

    ## False and True
  } else if (auto_actualizar == FALSE && silenciar_bitacora == TRUE) {

    publish_wiki(reporte, ruta_repo_wiki,
                 automatic_update = FALSE, quiet_render_logs = TRUE)


    ## True and True
  } else if (auto_actualizar == TRUE && silenciar_bitacora == TRUE) {

    publish_wiki(reporte, ruta_repo_wiki,
                 automatic_update = TRUE, quiet_render_logs = TRUE)


    ## False and False
  } else if (auto_actualizar == FALSE && silenciar_bitacora == FALSE) {

    publish_wiki(reporte, ruta_repo_wiki,
                 automatic_update = FALSE, quiet_render_logs = FALSE)
  }
}

