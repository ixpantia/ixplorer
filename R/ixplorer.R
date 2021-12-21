#' `ixplorer` package

#' \code{ixplorer.es} package
#' `ixplorer` package
#'
#' ixplorer for dataops
#'
#' See the README on
#'
#' @docType package
#' @name ixplorer
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(
  c(".", "assignee.login",
    "number", "title",
    "due_date", "hour",
    "borrar", "issue_url",
    "Due", "Nr",
    "persistence",
    "keyring",
    "key_get",
    "created_at",
    "updated_at",
    "closed_at",
    "user.last_login",
    "user.created",
    "milestone.due_on",
    "assignee.last_login",
    "assignee.created",
    "milestone.title",
    "path_wiki_repo"
    ))
