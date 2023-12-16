#' Create a new user on ixplorer.
#'
#' This function creates a new user on an ixplorer server by sending a POST
#' request to the appropriate endpoint.
#'
#' @import httr
#' @importFrom jsonlite
#' @importFrom purrr
#' @importFrom magrittr %>%
#' @import dplyr
#'
#'
#' @param base_url The base URL of the ixplorer server.
#' @param api_key The API key for authentication on the ixplorer server.
#' @param owner The owner (organization or user) of the repository where
#' the user will be created.
#' @param repo The name of the repository where the user will be created.
#' @param user_data An object containing user information, including
#' at least email, login_name, and username.
#'
#' @return A list containing the response from the ixplorer server,
#'  including the generated password.
#'
#' @export
utils::globalVariables(c("email", "login", "password", "report"))

new_user <- function(base_url, api_key, owner, repo, user_data) {
  if (missing(base_url)) {
    stop("Please add a valid URL")
  } else if (missing(api_key)) {
    stop("Please add a valid API token")
  } else if (missing(owner)) {
    stop("Please add a valid owner")
  } else if (missing(repo)) {
    stop("Please add a valid repository")
  } else if (missing(user_data)) {
    stop("Please add valid user data")
  }

  # URL Construction
  base_url <- sub("/$", "", base_url)
  gitea_url <- file.path(base_url, "api/v1", "admin", "users")
  authorization <- paste("token", api_key)

  # Function to generate a secure password
  generate_patterned_password <- function() {
    # Define character sets
    uppercase <- sample(LETTERS, 1)
    lowercase <- sample(letters, 7, replace = TRUE)
    numbers <- sample(0:9, 2, replace = TRUE)
    special_char <- sample(c("?", "!", ",", "&", "%", "$", "#", "@", "*"), 1)

    # Combine sets to form the password
    password_combined <- paste0(uppercase, paste0(lowercase, collapse = ""), paste0(numbers, collapse = ""), special_char)
    return(password_combined)
  }

  # Generate a secure password
  secure_password <- generate_patterned_password()

  # Add the password to user data
  user_data$password <- secure_password

  # Convert the user data to JSON
  user_data_json <- jsonlite::toJSON(user_data, auto_unbox = TRUE)

  # Send the POST request to create the user
  r <- tryCatch(
    POST(gitea_url,
         add_headers(Authorization = authorization,
                     "Content-Type" = "application/json"),
         body = user_data_json),
    error = function(cond) {"Failure"}
  )

  if (!inherits(r, "response")) {
    stop(paste0("Error posting to the URL: ", gitea_url))
  }

  # Handle HTTP errors
  if (http_type(r) >= 400) {
    stop_for_status(r)
  }

  # Parse the JSON response
  content_response <- jsonlite::fromJSON(content(r, as = "text"))
  content_response$password <- secure_password
  return(content_response)
}



#' Add a user as a collaborator to an ixplorer repository.
#'
#' This function adds a user as a collaborator to a specific repository on
#' ixplorer with specific permissions.
#'
#' @param base_url The base URL of the ixplorer server.
#' @param api_key The API key for authentication on the ixplorer server.
#' @param owner The owner (organization or user) of the repository where
#' the user will be added as a collaborator.
#' @param repo The name of the repository where the user will be added
#' as a collaborator.
#' @param username The username of the user to be added as a collaborator.
#' @param collaborator_permissions The permissions to be assigned
#' to the collaborator ("Read", "Write", or "Admin").
#'
#' @return The response from the ixplorer server.
#'

# Function to add a user to the repository with specific permissions
add_user_to_repo <- function(base_url, api_key, owner, repo, username, collaborator_permissions) {
  gitea_url <- file.path(base_url, "api/v1/repos", owner, repo, "collaborators", username)
  authorization <- paste("token", api_key)

  # Define the JSON request body that includes specific permissions
  permission_body <- jsonlite::toJSON(list(permission = collaborator_permissions), auto_unbox = TRUE)

  # Send the PUT request to add the user to the repository
  r <- tryCatch(
    PUT(gitea_url,
        add_headers(Authorization = authorization,
                    "Content-Type" = "application/json"),
        body = permission_body),
    error = function(cond) {"Failure"}
  )

  if (http_type(r) >= 400) {
    stop_for_status(r)
  }

  return(r)
}


#' Create users and add them to an ixplorer repository.
#'
#' This function automates the process of creating users and adding
#' them as collaborators to a repository on ixplorer.
#'
#' @param base_url The base URL of the ixplorer server.
#' @param api_key The API key for authentication on the ixplorer server.
#' @param owner The owner (organization or user) of the repository where
#'  the users will be added as collaborators.
#' @param repo The name of the repository where the users will be added
#' as collaborators.
#' @param user_data_list A list of objects, each containing user information
#' (email, login_name, username).
#' @param collaborator_permissions The permissions to be assigned
#' to the collaborators ("Read", "Write", or "Admin").
#'
#' @return A data frame containing information about the created users.
#'
#' @examples
#' \dontrun{
#'   # API configuration
#'   base_url <- "https://prueba.ixpantia.com"
#'   api_key <- "your_api_key" # Replace with your actual API key
#'   owner <- ""
#'   repo <- ""
#'
#'   # New user data (in a list)
#'   user_data_list <-
#'     list(
#'       email = "user@example.com",
#'       login_name = "User",
#'       username = "User"
#'     )
#'
#'   # Collaborator permissions to be added (Read, Write, or Admin)
#'   collaborator_permissions <- "Write"
#'
#'   # Create users and add them to the repository with specific permissions
#'   results <- create_users(base_url, api_key, owner, repo, user_data_list,
#'   collaborator_permissions)
#'   print(results)
#' }

# Define the function 'crear_datos'
create_users <- function(base_url, api_key, owner, repo, user_data_list, collaborator_permissions) {
  # Create users and add them to the repository with specific permissions
  results <- lapply(user_data_list, function(user_data) {
    user_response <- create_users(base_url, api_key, owner, repo, user_data)
    add_user_to_repo(base_url, api_key, owner, repo, user_data$username, collaborator_permissions)
    return(user_response)
  })

  # Convert the list of results to a DataFrame
  df <- dplyr::bind_rows(results)

  # Select only the desired columns
  df <- dplyr::select(df, email, login, username, password)

  return(df)
}

