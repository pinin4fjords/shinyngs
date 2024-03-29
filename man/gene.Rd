% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gene.R
\name{gene}
\alias{gene}
\title{The server function of the gene module}
\usage{
gene(input, output, session, eselist)
}
\arguments{
\item{input}{Input object}

\item{output}{Output object}

\item{session}{Session object}

\item{eselist}{ExploratorySummarizedExperimentList object containing
ExploratorySummarizedExperiment objects}
}
\description{
The gene module picks specified rows out the assay data, either simply by id
or label. This is used to create a gene-centric info page.
}
\details{
This function is not called directly, but rather via callModule() (see
example).
}
\examples{
callModule(gene, "gene", eselist)

}
\keyword{shiny}
