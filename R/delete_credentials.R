#' Delete your ixplorer credentials
#' @details Delete the credentials saved by add_token() in your computer.
#' This can be useful when you are using the ixplorer package on a
#' shared computer or any other case you don't want to keep the credentials
#' stored
#'
#' @param instance an instance from ixplorer such as "prueba" or "masterclass"
#'
#' @return Delete credentials saved by add_token()
#'
#'
#' @examples
#' \dontrun{
#' delete_credentials("masterclass")
#' }
#' @export
delete_credentials <- function(instance = "current"){

 # When the default value is selected
  if (instance == "current"){

    # Check for an active instance in environment
    if (Sys.getenv("ixplorer_instance") != ""){

      # Gets the instance from the environment and deletes it
      # and unsets the enviroment variable
      found_instance <- Sys.getenv("ixplorer_instance")

      keyring::keyring_delete(found_instance)

      Sys.unsetenv("ixplorer_instance")

      message(found_instance, " was deleted")

  # When there is no instance in the environment

    } else {

      message("Couldn't find a current instance in the enviroment \n
      Specify the instance you want to delete or try the authetication gadget")

    }

 # When the user specifies an instance other than the default value
  } else {

    # Defines the instance specified and searches for it in the saved instances

    specific_instance <- paste0("ixplorer_", instance)

    found_instances <- keyring::keyring_list() %>%
      filter(stringr::str_detect(keyring, specific_instance))

    # Check for the instance specified in the saved instances.
    if (nrow(found_instances) > 0){

      # Deletes the instance specified
      keyring::keyring_delete(found_instances[1,1])

      message(found_instances[1,1], " was deleted")

    } else {

      message("There is no saved instance with the name: ", specific_instance)

    }
  }
}


#' Elimina tus credenciales de ixplorer
#' @details Borre las credenciales guardadas por add_token() en su computadora.
#' Esto puede ser útil cuando está usando el paquete ixplorer en una
#' computadora compartida o en cualquier otro caso en el que no desee mantener
#' las credenciales almacenadas.
#'
#' @param instance una instancia de ixplorer como "masterclass" o "prueba"
#'
#' @return Elimina las credenciales guardadas por add_token()
#'
#'
#' @examples
#' \dontrun{
#' delete_credentials("masterclass")
#' }
#' @export
eliminar_credenciales <- function(instancia = "actual"){

  if (instancia == "actual"){

    delete_credentials(instance = "current")
  } else {

    delete_credentials(instance = instancia)
  }

}
