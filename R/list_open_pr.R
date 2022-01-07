#' List open pull requests
#' @import purrr
#'
#' @description Lists your open pull requests in ixplorer
#'
#'
#' @param instance an instance from ixplorer such as "masterclass" or "prueba".
#' Default value "all" lists the pull requests of all  your saved instances.
#' Value "saved" lists pull requests from your current instance
#' @param assignee name of a person from your team. Default "me" lists the
#' pull requests assign to you
#'
#' @return tibble of list instances
#' @export
#'
#' @examples
#' \dontrun{
#' list_open_pr(instance = "prueba",
#'              assignee = "daniel")
#' }
list_open_pr <- function(instance = "all", assignee = "me"){

  # Saved instances -----------------------------------------------------------

  saved_instances <- keyring::keyring_list() %>%
    filter(stringr::str_detect(keyring, "ixplorer_"))

  # Look for instance ---------------------------------------------------------

  if (instance == "saved") {

    # It looks in session
    if (Sys.getenv("ixplorer_instance") != "") {

      my_instance <- Sys.getenv("ixplorer_instance")


      # If there is no enviroment variable it means user is looking for
      # a previously saved instance
    } else if (Sys.getenv("ixplorer_instance") == "") {

      # if there are saved instances, then it chooses the instance that was last saved
      if (nrow(saved_instances) > 0) {

        last_saved <- saved_instances[1,1]
        my_instance <- last_saved


        # When there are no saved instances, then a message is printed
      } else {
        stop("There are no saved instances")
      }

    }

    # If the user chooses an instance other than "saved" then it looks for
    # the specified instance in previously saved keyrings

  } else if( instance == "all" ){

    my_instance = "all"

    # If the user chooses an instance other than "saved" then it looks for
    # the specified instance in previously saved keyrings

  } else {

    specific_instances <- saved_instances %>%
      filter(keyring == paste0("ixplorer_",instance))

    if (nrow(specific_instances) > 0) {

      my_instance <- paste0("ixplorer_", instance)

    } else {

      stop("No credentials for ", instance)

    }

    }

  # filter the saved instances according to selection--------------------------
if(instance != "all") {

  saved_instances <- saved_instances %>%
    filter(keyring == my_instance) %>%
    rename(instance = keyring)


} else {

  saved_instances <- saved_instances %>%
    rename(instance = keyring)
}



  #get links ------------------------------------------------------------------
  # For every row in saved instances we get the corresponding url
  saved_links <- saved_instances %>%
    purrr::pmap_dfr(function(...) {

      current_row <- tibble(...)
      ix_link <- keyring::key_get("ixplorer_link",
                                  keyring = current_row$instance)

      current_row %>%
        mutate(link = ix_link)
    })

  # get_projects --------------------------------------------------------------
  # For every row in saved_links we get a the list of project names

  my_projects <- saved_links %>%
    purrr::pmap_dfr(function(...) {

      current <- tibble(...)

      ix_instance <- current$instance
      ix_link <- current$link
      ix_token <- keyring::key_get("ixplorer_token",
                                   keyring = ix_instance)


      project_list <- gitear::get_organizations(base_url = ix_link,
                                                api_key = ix_token)

      all_projects <- list(project_list$username)

      current %>%
        mutate(projects = all_projects)
    })

  # Project data --------------------------------------------------------------
  # get each project into a single row

  saved_projects <- my_projects %>%
    tidyr::unnest(projects)

   # get repos ----------------------------------------------------------------
   # for every row with a project we get the list of repos from each project


  my_repos <- saved_projects %>%
    purrr::pmap_dfr(function(...) {

      current <- tibble(...)

      ix_instance <- current$instance
      ix_link <- current$link
      ix_token <- keyring::key_get("ixplorer_token",
                                   keyring = ix_instance)
      ix_project <- current$projects


      repos <- gitear::get_list_repos_org(base_url = ix_link,
                                          api_key = ix_token,
                                          org = ix_project)
      repos <- repos %>%
        filter(!is.na(open_pr_counter) && open_pr_counter > 0)

      all_repos <- list(repos$name)

      current %>%
        mutate(repo = all_repos)
    })

  # Repo data
  # Ordering each project into a single row

  saved_repos <- my_repos %>%
    tidyr::unnest(repo)

  # Check instances in saved_repos

  if( instance != "all"){
    if(!(instace %in% saved_repos$instance)){
      stop("No pull requests from ", instance )
    }
  }

  #get pull requests
  # Loop to bind all pull request data from each repo in saved_repos

  all_prs <- function(x){

    all_prs <- NULL

    for (i in 1:nrow(x)){

      ix_link <- toString(x[i,4])
      ix_token <- keyring::key_get("ixplorer_token",
                                   keyring = toString(x[i,1]))
      ix_project <- toString(x[i,5])
      ix_repo <- toString(x[i,6])


      pr <- gitear::get_pull_requests(base_url = ix_link,
                                      api_key = ix_token,
                                      owner = ix_project,
                                      repo = ix_repo)

      all_prs <- bind_rows(all_prs, pr)

    }

    return(all_prs)

  }


  # pull request data selection -----------------------------------------------

  pullr_data <- all_prs(saved_repos) %>%
    filter(state == "open") %>%
    rename(pr_id = id) %>%
    tidyr::unnest(assignee) %>%
    mutate(url = stringr::str_remove(url, "https://")) %>%
    mutate(url = stringr::str_remove(url, ".com")) %>%
    tidyr::separate(col = url, into = c("pr_instance", "pr_project", "pr_repo",
                                        "type", "type_number"), sep = "/") %>%
    select(pr_instance, pr_project, pr_repo, number, title, login) %>%
    rename(pr_assignee = login) %>%
    rename(pr_title = title) %>%
    mutate(pr_instance = stringr::str_replace(pr_instance,".i","_i" ))


  # Filter data according to assignee specification
  # When team is chosen data stay as is


  if (assignee == "team"){

    pull_request_data <- pullr_data

    #When "me" is chosen we get the saved username and filter
  } else if (assignee == "me"){

    ix_instance <- toString(saved_instances[1,1])
    ix_username <- keyring::key_get("ixplorer_user_name",
                                    keyring = ix_instance)

    pull_request_data <- pullr_data %>%
      filter(pr_assignee == ix_username)
    # when other assignee is chosen we filter with that name

  } else if (assignee != "team" && assignee != "team"){

    pull_request_data <- pullr_data %>%
      filter(pr_assignee == assignee)


  }



return(pull_request_data)

}




