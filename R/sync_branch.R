#' Synch your master branch with upstream
#' @import gert
#' @description Runs steps to synch your master branch to upstream
#' automatically
#'
#' @param default_branch_name defines a default branch name to be able to
#' synchronize it
#' @return No return value, called for side effects
#' @export
synch_branch <- function(default_branch_name = "master") {

  remotes <- gert::git_remote_list(repo = ".")

  if (("upstream" %in% remotes$name) == TRUE) {

    gert::git_fetch(remote = "upstream")
    gert::git_branch_checkout(branch = default_branch_name)
    gert::git_rebase_commit(paste0("upstream/", default_branch_name))
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
#' @param rama_por_defecto define la rama por defecto que queremos actualizar
#' @return No hay valor de retorno - se llama por su efecto secundario
#' @export
actualizar_rama <- function(rama_por_defecto) {

  synch_branch(default_branch_name = rama_por_defecto)

}

