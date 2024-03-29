% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geneselect.R
\name{geneselectInput}
\alias{geneselectInput}
\title{The UI input function of the geneselect module}
\usage{
geneselectInput(id, select_genes = TRUE)
}
\arguments{
\item{id}{Submodule namespace}

\item{select_genes}{Disable gene (row) - wise selection if set to FALSE}
}
\value{
output An HTML tag object that can be rendered as HTML using
as.character()
}
\description{
This module provides controls for selecting genes (matrix rows) by various
criteria such as variance and gene set.
}
\details{
This will generally not be called directly, but by other modules such as the
heatmap module.
}
\examples{
geneselectInput("myid")

}
\keyword{shiny}
