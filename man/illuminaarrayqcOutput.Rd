% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/illuminaarrayqc.R
\name{illuminaarrayqcOutput}
\alias{illuminaarrayqcOutput}
\title{The output function of the illuminaarrayqc module}
\usage{
illuminaarrayqcOutput(id)
}
\arguments{
\item{id}{Submodule namespace}
}
\value{
output An HTML tag object that can be rendered as HTML using
as.character()
}
\description{
This module plots control probes from an illumina microarray experiment.
}
\details{
This funcion provides the form elements to control the display

This provides actual plot element for display by applications
}
\examples{
illuminaarrayqcOutput("myid")

}
\keyword{shiny}
