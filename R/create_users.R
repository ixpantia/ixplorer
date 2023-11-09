#' Create a new users in an ixplorer repository
#'
#' This function creates a new users in an ixplorer repository and adds them with specific permissions.
#'
#' @param base_url Base URL of the ixplorer server.
#' @param api_key API key for authentication.
#' @param owner Owner's name of the repository.
#' @param repo Repository name.
#' @param user_data Users data in the form of a list.
#' @param collaborator_permissions Collaborator permissions (Read, Write, or Admin).
#' @return A data frame with the information of the created users, including email, login, username, and password.
#'
#' @examples
#' \dontrun{
#' # API configuration
#' base_url <- "https://prueba.com"
#' api_key <- "your_api_key"
#' owner <- "repository_owner"
#' repo <- "repository_name"
#'
#' # New users data (in a list)
#' user_data_list <- list(
#'   list(
#'     email = "user@example.com",
#'     login_name = "User-1",
#'     username = "User-1"
#'   ),
#'   list(
#'     email = "user2@example.com",
#'     login_name = "User-2",
#'     username = "User-2"
#'   )
#' )
#'
#' # Collaborator permissions to be added (Read, Write, or Admin)
#' collaborator_permissions <- "Write"
#'
#' # Create users and add them to the repository with specific permissions
#' results <- create_users(base_url, api_key, owner, repo, user_data_list, collaborator_permissions)
#'
#' # Print the results
#' print(results)
#' }
#'

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

  # Add the password to users data
  user_data$password <- secure_password

  # Convert the users data to JSON
  user_data_json <- jsonlite::toJSON(user_data, auto_unbox = TRUE)

  # Send the POST request to create the user
  r <- tryCatch(
    POST(gitea_url,
         add_headers(Authorization = authorization,
                     "Content-Type" = "application/json"),
         body = user_data_json),
    error = function(cond) {"Failure"}
  )

  if (class(r) != "response") {
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


# Function to add a users to the repository with specific permissions
add_user_to_repo <- function(base_url, api_key, owner, repo, username, collaborator_permissions) {
  gitea_url <- file.path(base_url, "api/v1/repos", owner, repo, "collaborators", username)
  authorization <- paste("token", api_key)

  # Define the JSON request body that includes specific permissions
  permission_body <- jsonlite::toJSON(list(permission = collaborator_permissions), auto_unbox = TRUE)

  # Send the PUT request to add the users to the repository
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

# Define the function create_users
create_users <- function(base_url, api_key, owner, repo, user_data_list, collaborator_permissions) {
  # Create users and add them to the repository with specific permissions
  results <- lapply(user_data_list, function(user_data) {
    user_response <- new_user(base_url, api_key, owner, repo, user_data)
    add_user_to_repo(base_url, api_key, owner, repo, user_data$username, collaborator_permissions)
    return(user_response)
  })

  # Convert the list of results to a DataFrame
  df <- dplyr::bind_rows(results)

  # Select only the desired columns
  df <- dplyr::select(df, email, login, username, password)

  return(df)
}
