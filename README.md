# ixplorer (Español)

El paquete ixplorer para llevar funcionalidad de ixplorer cerca al cliente. 

## Instalación

Gracias por hacer uso de ixplorer. Para hacer uso del paquete ixplorer es 
necesario contar el paquete *gitear*. Si no lo hemos instalado, con la siguiente
instrucción podremos lograrlo:

```
devtools::install_github("ixpantia/gitear")
```

En caso de no contar con el paquete devtools instalado, lo podemos realizar con:

```
install.packages("devtools")
```

Para instalar el paquete *ixplorer* existen dos formas de hacerlo: 
  1 - La primera consiste en la instalación de un archivo comprimido cuyo
  paquete es la última versión revisada y funcional.
  2 - La segunda manera para su instalación es la versión en desarrollo que 
  podría contener nuevas funcionalidades sin embargo no se garantiza que esté
  libre de errores.


#### Instalación última versión:

```
devtools::install_url("https://storage.googleapis.com/ixplorer/ixplorer_0.0.2.tar.gz")
```
#### Instalación versión en desarrollo:

Podemos copiar y pegar el siguiente código:
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
Si nos pide instalar el paquete *rstudioapi* lo podemos realizar con
`install.packages("rstudioapi")`

Después de copiar y pegar el código en la consola y hacerlo correr, verá un 
cuadro de diálogo pidiendo su usuario. Por favor complete el espacio con su 
nombre de usuario de ixplorer.

El segundo paso será un cuadro preguntando por su contraseña. Por favor llene
el cuadro con su contraseña de ixplorer.

##### ¿Dónde puedo encontrar mi nombre de usuario y contraseña para ixplorer?

Su proveedor del servicio o bien supervisor de proyectos de ciencia de datos
debe de otorgarle los derechos respectivos.

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
