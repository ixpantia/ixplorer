% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete_credentials.R
\name{eliminar_credenciales}
\alias{eliminar_credenciales}
\title{Elimina tus credenciales de ixplorer}
\usage{
eliminar_credenciales(instancia = "actual")
}
\arguments{
\item{instancia}{una instancia de ixplorer como "masterclass" o "prueba"}
}
\value{
Elimina las credenciales guardadas por add_token()
}
\description{
Elimina tus credenciales de ixplorer
}
\details{
Borre las credenciales guardadas por add_token() en su computadora.
Esto puede ser útil cuando está usando el paquete ixplorer en una
computadora compartida o en cualquier otro caso en el que no desee mantener
las credenciales almacenadas.
}
\examples{
\dontrun{
delete_credentials("masterclass")
}
}
