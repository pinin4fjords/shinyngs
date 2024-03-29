% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genesetselect.R
\name{genesetselect}
\alias{genesetselect}
\title{The server function of the genesetselect module}
\usage{
genesetselect(
  input,
  output,
  session,
  eselist,
  getExperiment,
  multiple = TRUE,
  filter_by_type = FALSE,
  require_select = TRUE
)
}
\arguments{
\item{input}{Input object}

\item{output}{Output object}

\item{session}{Session object}

\item{eselist}{An ExploratorySummarizedExperimentList with its gene_sets
slot set}

\item{getExperiment}{Accessor for returning an
ExploratorySummarizedExperiment object, with 'labelfield' set in its slots}

\item{multiple}{Boolean: should it be possible to select multiple gene sets?}

\item{filter_by_type}{Provide a filter for gene set type?}

\item{require_select}{Require a gene set selection?}
}
\value{
output A list of two reactive functions which will be used by other
modules.
}
\description{
The gene set module is for adding a gene set filter to displays. A
\code{\link[shiny]{selectizeInput}} is used for performance reasons,
providing an autocomplete field for selecting from a list that could stretch
to thousands of entries. This would be difficult to do client-side using a
standard select field.
}
\examples{
geneset_functions <- callModule(genesetselect, "heatmap", getExperiment)

}
\keyword{shiny}
