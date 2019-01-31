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

Thanks for using ixplorer. To completely use the ixplorer package you will also
need to install the **gitear** package. If you haven't install it yet, with this
instruction you can do it:

```
devtools::install_github("ixpantia/gitear")
```
In case you do not have the devtools package installed on your computer, you
can do it with:

```
install.packages("devtools")
```

To install the *ixplorer* package, we have two ways to do it:
  1 - The first one consist in the installation of a zip file, in which the 
  the content is the latest package version reviewed and functional.
  2 - The second one is the develop version that could have new functionalities,
  nonetheless it's no guaranted that it's error free.

#### Last version installation:

```
devtools::install_url("https://storage.googleapis.com/ixplorer/ixplorer_0.0.2.tar.gz")
```
#### Dev version installation:

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

##### Where can I find my username and password for ixplorer?
Your provider or data science project manager should provide you with this
credentials. 
