#' @title Set git credential cache time
#' @description Set the amount of time that the git credentials will be
#' available on the system you are working on.
#'
#' @details As we move between servers to work we oftentimes do not want to
#' store our git passwords there. For instance when we are workign on servers
#' of third parties. The extreme is to include the username and password for
#' every single git call. But with git we can have a middle way where we store
#' the credentials in cache for a limited amount of time.
#'
#' @param hours the amount of hours that the credentials should be stored.
#' Defaults to 4 hours.
#' @param global if TRUE then the setting will be sett as a global setting. If
#' FALSE it will only be for the current git repository
#'
#' @export
set_git_credentials_cache <- function(hours = 4, global = TRUE) {

  if (global == TRUE) {
    system(paste0('git config --global credential.helper --replace-all "cache --timeout=',
                  hours * 60 * 60, '"'))
  } else {
    system(paste0('git config credential.helper "cache --timeout=',
                  hours * 60 * 60, '"'))
  }
}
