# ixplorer

El paquete ixplorer para llevar funcionalidad de ixplorer cerca al cliente. 

## Installation 

For the installation copy and paste the following in your console and press enter

```
install_ixplorer <- function(){
  usr <- rstudioapi::askForSecret(message = "Please enter your username")
  pw <- rstudioapi::askForPassword("Please enter your password")
  devtools::install_git("https://secure.ixpantia.com/ixplorer/ixplorer.git", 
                        branch = "master", 
                        credentials = git2r::cred_user_pass(
                          user = usr, password = pw),
                        build_vignettes = TRUE)
}
```

After running the function, in your console run:

```
install_ixplorer()
```

Fill the first box with your ixplorer username and the second box with your
ixplorer password.

This is going to install the latest version of the ixplorer package.
