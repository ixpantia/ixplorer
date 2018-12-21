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

install_ixplorer()
```

After you copy and paste this on your console you will see at first a box asking
for your username. Please complete this with your ixplorer username. 

The next step is a second box asking for your password. Please fill this with
your ixplorer password.

## Where can I find my username and password for ixplorer?
Your provider or data science project manager should provide you with this
credentials. 


This is going to install the latest version of the ixplorer package.
