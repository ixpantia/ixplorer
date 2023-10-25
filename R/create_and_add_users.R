# Common Arguments
#' Load necessary libraries
#'
#' @import httr
#' @import jsonlite
#' @import gitear
#' @import password
#'
#' @rdname common_args
#' @param base_url The base URL of the Gitea instance.
#' @param api_key The API key for authentication.
#' @param owner The owner of the repository.
#' @param repo The name of the repository.

#' @title Load Necessary Libraries
#'
#' @description
#' This function loads the necessary libraries for the code to work.
#'
#' @seealso
#' Other functions that require these libraries.
#'
#' @examples
#' \dontrun{
#' @title Define Key Parameters for Gitea Instance
#'
#' @description
#' This function sets up key parameters required to interact with a Gitea instance.
#'
#' @examples
#' \dontrun{
#' define_parameters("https://example.com", "API_KEY", "owner", "repo")
#' }
#' @inheritParams common_args
define_parameters <- function(base_url, api_key, owner, repo) {
  base_url <<- base_url
  api_key <<- api_key
  owner <<- owner
  repo <<- repo
}

#' @title Create Users on Ixpantia
#'
#' @description
#' This function creates users on the Ixpantia Gitea instance.
#'
#' @param user_data A list containing user data.
#'
#' @return A list with user information.
#'
#' @examples
#' \dontrun{
#' create_users_ixpantia(user_data)
#' }
#' @inheritParams common_args
create_users_ixpantia <- function(user_data) {
  # Ensure all necessary arguments are provided
  if (missing(base_url)) {
    stop("Please add a valid URL")
  } else if (missing(api_key)) {
    stop("Please add a valid API token")
  }
  else if (missing(owner)) {
    stop("Please add a valid owner")
  }
  else if (missing(repo)){
    stop("Please add a valid repository")
  }
  else if (missing(user_data)) {
    stop("Please add valid user data")
  }

  # Construct the URL for the API endpoint
  base_url <- sub("/$", "", base_url)
  gitea_url <- file.path(base_url, "api/v1", "admin", "users")
  authorization <- paste("token", api_key)

  # Inner function to generate a patterned password
  generate_patterned_password <- function() {
    # Define character sets
    uppercase <- sample(LETTERS, 1)
    lowercase <- sample(letters, 7, replace = TRUE)
    numbers <- sample(0:9, 2, replace = TRUE)
    special_char <- sample(c("?", "!", "&", "%", "$", "#", "@", "*"), 1)

    # Combine sets to create the password
    password_combined <- paste0(uppercase, paste0(lowercase, collapse = ""),
                                paste0(numbers, collapse = ""), special_char)
    return(password_combined)
  }

  # Generate a password for the new user
  secure_password <- generate_patterned_password()

  # Add the generated password to the user data
  user_data$password <- secure_password

  # Convert the user data into JSON format
  user_data_json <- jsonlite::toJSON(user_data, auto_unbox = TRUE)

  # Make the POST request to create the user
  r <- tryCatch(
    POST(gitea_url,
         add_headers(Authorization = authorization,
                     "Content-Type" = "application/json"),
         body = user_data_json),
    error = function(cond) {"Failure"}
  )

  # Print the raw HTTP response
  print(r)

  # Check if the response is valid
  if (class(r) != "response") {
    stop(paste0("Error posting to the URL: ", gitea_url))
  }

  # Convert any HTTP errors into R errors
  stop_for_status(r)

  # Extract content from the response
  content_response <- jsonlite::fromJSON(content(r, as = "text"))
  content_response$password <- secure_password
  print(content_response) # Print the content of the response
  return(content_response)
}

#' @title Add User as a Collaborator to a Repository
#'
#' @description
#' This function adds a user as a collaborator to a repository in the Gitea instance.
#'
#' @param username The username of the user to add as a collaborator.
#'
#' @return The HTTP response from the API.
#'
#' @examples
#' \dontrun{
#' add_user_to_repo("username")
#' }
#' @inheritParams common_args
add_user_to_repo <- function(username) {
  # Construct the URL for the API endpoint
  gitea_url <- file.path(base_url, "api/v1/repos", owner, repo, "collaborators",
                         username)
  authorization <- paste("token", api_key)

  # Make the PUT request to add the user to the repo
  r <- PUT(gitea_url,
           add_headers(Authorization = authorization,
                       "Content-Type" = "application/json"))

  if(http_error(r)) {
    message("HTTP error occurred.")
    stop_for_status(r)
  }

  return(r)
}

#' @title Create and Add User to Repository
#'
#' @description
#' This function combines the user creation and repository collaboration functions.
#'
#' @param user_data A list containing user data.
#'
#' @return A list with user and repository response information.
#'
#' @examples
#' \dontrun{
#' create_and_add_user_to_repo(user_data)
#' }
#' @inheritParams common_args
create_and_add_user_to_repo <- function(user_data) {
  # First, create the user
  user_response <- create_users_ixpantia(user_data)

  # Then, add the user to the repository
  repo_response <- add_user_to_repo(user_response$response$username)

  # Return both responses
  return(list(user = user_response, repo = repo_response))
}
