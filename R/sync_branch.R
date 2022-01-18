#' Synch your master branch with upstream
#' @import gert
#' @description Runs steps to synch your master branch to upstream
#' automatically
#'
#' @return
#' @export
synch_branch <- function() {

  remotes <- gert::git_remote_list(repo = ".")

  if (("upstream" %in% remotes$name) == TRUE) {

    gert::git_fetch(remote = "upstream")
    gert::git_branch_checkout(branch = "master")
    gert::git_rebase_commit("upstream/master")
    gert::git_push()

  } else {
    message("No upstream in remote list, try add_upstream()")
  }
}



#' Actualice la rama master desde el upstream
#'
#' @description actualiza su rama master con su upstream
#' por medio de git rebase
#'
#' @export
actualizar_rama <- function() {

  synch_branch()

}















