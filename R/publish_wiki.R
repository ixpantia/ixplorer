#' Publish an rmarkdown into wiki repo
#'
#' @title Publish into wiki repo
#' @description Publish an specific rmarkdown into wiki repository, specifying
#' the path of the wiki repository once that you already clone this repository
#' into your machine. See Details if is your first time publishing in a wiki
#' repository.
#'
#' @details If this is the first time you are going to publish in a wiki, you must
#' follow these steps:
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
#' @param rmarkdown the path of rmarkdown file
#' @param path_wiki_repo the complete path of wiki repository.
#' @param automatic_update if you like to do automatically the pull, commit and
#'  push set in TRUE, if you like to do manually, set FALSE. Default is TRUE.
#' @param quiet_render_logs if you like to see the render logs of rmarkdown,
#'  set in TRUE, in otherwise set FALSE. Default is TRUE.
#' @return Publish a md into wiki repository
#'
#' @examples
#'
#' \dontrun{
#' publish_wiki(rmarkdown = "eda.Rmd",
#'              path_wiki_repo = "/home/cliente/proyecto/wiki_repo")
#' }
#'
#' @export
publish_wiki <- function(rmarkdown, path_wiki_repo, automatic_update = TRUE,
                         quiet_render_logs = FALSE) {

  # Repare path_wiki_repo

  if (stringr::str_sub(path_wiki_repo,
                       start = stringr::str_length(path_wiki_repo)) == "/") {

    path_wiki_repo <- stringr::str_sub(path_wiki_repo,
                                       start = 1,
                                       end = stringr::str_length(path_wiki_repo) - 1)
  }

  # Make pull if automatic_update is TRUE

  if (automatic_update) {

    if (nrow(gert::git_status(repo = path_wiki_repo)) > 0) {
      gert::git_stash_save(repo = path_wiki_repo)
      message("Some of your changes to the wiki repository were put into stash. If you want to retrieve them you must go to the wiki repository and run: git stash apply")
    }

    gert::git_pull(repo = path_wiki_repo)
  }

  # Render the rmarkdown to github_document output

  rmarkdown::render(rmarkdown, 'github_document', quiet = quiet_render_logs)

  # Copy md to wiki repository

  base_name <- stringr::str_remove_all(basename(rmarkdown), ".Rmd")

  md <- paste0(base_name, ".md")

  path_md <- paste0(stringr::str_remove_all(rmarkdown, basename(rmarkdown)), md)

  file.copy(path_md, path_wiki_repo, overwrite = TRUE)
  file.remove(path_md)

  # Copy images to wiki repository

  files_folder <- paste0(base_name, "_files")

  files_path <- paste0(stringr::str_remove_all(rmarkdown, basename(rmarkdown)),
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
                       base_name, ".Rmd"))
      }

    } else {

      message(paste0("There are no new changes or something that can be sent in ",
                     base_name, ".Rmd"))
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
        "git push")

    } else {

      cat(
        paste("Go to the repository", path_wiki_repo,
              "and execute the following commnads in terminal:"), "\n", "\n",
        paste("git pull"), "\n",
        paste("git add", md), "\n",
        paste0("git commit -m 'Update ", base_name, "'"), "\n",
        "git push")

    }

  }

}

