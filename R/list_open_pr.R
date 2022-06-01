#' List open pull requests
#' @import purrr
#'
#' @description Lists your open pull requests in ixplorer
#'
#'
#' @param instance an instance from ixplorer such as "masterclass" or "prueba".
#' Default value "all" lists the pull requests of all  your saved instances.
#' Value "saved" lists pull requests from your current instance
#' @param assignee name of a person from your team. Default "team" lists all
#' the pull requests in the instance with all assignees. When the value is "me"
#' it filters the pull requests list for assigned to you according to your username
#' provided in add_token()
#'
#' @return tibble of list instances
#' @export
#'
#' @examples
#' \dontrun{
#' list_open_pr(instance = "prueba",
#'              assignee = "daniel")
#' }
list_open_pr <- function(instance = "all", assignee = "team"){

# Look for instance -------------------------------------------------------


if (instance == "all") {

  saved_instances <- keyring::keyring_list() %>%
    filter(stringr::str_detect(keyring, "ixplorer_")) %>%
    rename(instance = keyring)

} else if (instance == "saved") {

  current_instance <- get_instance()

  if (current_instance != "none") {

    saved_instances <- keyring::keyring_list() %>%
      filter(keyring == current_instance) %>%
      rename(instance = keyring)

  } else {

    message("There are no saved instances")

  }

} else {

  ix_instance <- paste0("ixplorer_", instance)

  saved_instances <- keyring::keyring_list() %>%
    filter(keyring == ix_instance) %>%
    rename(instance = keyring)

  if (nrow(saved_instances) == 0) {

    stop("There is no instance named ", instance)
  }
}


# get urls ----------------------------------------------------------------


  # For every row in saved instances we get the corresponding url
  saved_links <- saved_instances %>%
    purrr::pmap_dfr(function(...) {

      current_row <- tibble(...)
      ix_link <- keyring::key_get("ixplorer_link",
                                  keyring = current_row$instance)

      current_row %>%
        mutate(link = ix_link)
    })


# get repos ---------------------------------------------------------------

  repos <- saved_links %>%
    purrr::pmap_dfr(function(...){

      current_row <- tibble(...)
      ix_link <- current_row$link
      ix_token <- keyring::key_get("ixplorer_token",
                                   keyring = current_row$instance)
      repositories <- gitear::get_repositories(base_url = ix_link,
                                               api_key = ix_token)
      repo_list <- list(repositories)

      current_row %>%
        mutate(repositories = repo_list) %>%
        tidyr::unnest(repositories) %>%
        tidyr::unnest(data.owner) %>%
        select(instance,username, data.name,
               data.fork, data.open_pr_counter) %>%
        rename(project = username) %>%
        rename(repository = data.name) %>%
        filter(data.fork == FALSE & data.open_pr_counter > 0)

      })


# get gitea pull requests  ------------------------------------------------


  my_prs <- repos %>%
    purrr::pmap_dfr(function(...) {


      current <- tibble(...)


      ix_instance <- current$instance
      ix_project <- current$project
      ix_repo <- current$repository


      ix_link <- keyring::key_get("ixplorer_link",
                                  keyring = ix_instance)
      ix_token <- keyring::key_get("ixplorer_token",
                                   keyring = ix_instance)

      pr <- gitear::get_pull_requests(base_url = ix_link,
                                      api_key = ix_token,
                                      owner = ix_project,
                                      repo = ix_repo)
      all_prs <- list(pr)

      current %>%
        mutate(prs = all_prs) %>%
        tidyr::unnest(prs) %>%
        select( number, title, assignees,instance, project, repository) %>%
        tidyr::unnest(assignees) %>%
        rename(pr_assignee = username) %>%
        rename(pr_number = number) %>%
        rename(pr_title = title) %>%
        rename(pr_project = project) %>%
        rename(pr_repo = repository) %>%
        mutate(instance = stringr::str_remove(instance, "ixplorer_")) %>%
        select(pr_number, pr_title, pr_assignee, pr_project, pr_repo, instance)

    })


# filter for assignee -----------------------------------------------------



  if (assignee == "team") {

    pull_request_data <- my_prs

    #When "me" is chosen we get the saved username and filter
  } else if (assignee == "me") {

    ix_instance <- toString(saved_instances[1,1])
    ix_username <- keyring::key_get("ixplorer_user_name",
                                    keyring = ix_instance)

    pull_request_data <- my_prs %>%
      filter(pr_assignee == ix_username)
    # when other assignee is chosen we filter with that name

  } else if (assignee != "team" && assignee != "team") {

    pull_request_data <- my_prs %>%
      filter(pr_assignee == assignee)

  }


# Return pull request data ------------------------------------------------
return(pull_request_data)

}

