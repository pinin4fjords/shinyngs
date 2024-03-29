% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleselect.R
\name{sampleselectInput}
\alias{sampleselectInput}
\title{The UI input function of the sampleselect module}
\usage{
sampleselectInput(id, eselist, getExperiment, select_samples = TRUE)
}
\arguments{
\item{id}{Submodule namespace}

\item{eselist}{ExploratorySummarizedExperimentList object containing
ExploratorySummarizedExperiment objects}

\item{getExperiment}{Reactive expression that returns a
\code{ExploratorySummarizedExperiment} with assays and metadata. Usually a
result of a user selection}

\item{select_samples}{Select samples at all? If set to false, a hidden input
indicating the selection of all samples is produced.}
}
\value{
output An HTML tag object that can be rendered as HTML using
  as.character()
}
\description{
This module provides controls for selecting matrix columns by sample or group
name.
}
\details{
This will generally not be called directly, but by other modules such as the
selectmatrix module.
}
\examples{
sampleselectInput(ns("heatmap"))

}
\keyword{shiny}
