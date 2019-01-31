# ixplorer (Español)

El paquete ixplorer para llevar funcionalidad de ixplorer cerca al cliente. 

## Instalación

Gracias por hacer uso de ixplorer. Las siguientes son instrucciones para su 
correcta instalación. Estas instrucciones han de ser usadas desde R. Una vez instalados los paquetes podrá hacer uso de las utilidades del ixplorer.

Como primer paso debemos de instalar el paquete gitear desde github. Si ya
tenemos este paquete instalado no debemos de correr la instrucción

```
devtools::install_github("ixpantia/gitear")
```

En caso de no contar con el paquete devtools instalado, lo podemos realizar con:

```
install.packages("devtools")
```

El segundo paso es instalar el paquete ixplorer, lo cual podemos realizar con la instrucción:

```
devtools::install_url("https://storage.googleapis.com/ixplorer/ixplorer_0.0.2.tar.gz")
```

Una vez concluidos estos pasos seremos capaces de aprovechar las utilidades de ixplorer.

# ixplorer (english)

The ixplorer package takes the functionality of ixplorer close to the client.

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
