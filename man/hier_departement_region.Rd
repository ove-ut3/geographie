% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hierarchie.R
\name{hier_departement_region}
\alias{hier_departement_region}
\title{Obtenir le code region a partir d'un code departement}
\usage{
hier_departement_region(code_departement)
}
\arguments{
\item{code_departement}{Un vecteur de code département.}
}
\value{
Un vecteur de code de région.

Jeu de données source : \code{geographie::departement}.\cr
}
\description{
Obtenir le code région à partir d'un code département.
}
\examples{
geographie::hier_departement_region(c("003", "056"))

}
