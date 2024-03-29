% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexseqplot.R
\name{dexseqplotInput}
\alias{dexseqplotInput}
\title{The UI input function of the dexseqplot Shiny module}
\usage{
dexseqplotInput(id, eselist)
}
\arguments{
\item{id}{Submodule namespace}

\item{eselist}{ExploratorySummarizedExperimentList object containing
ExploratorySummarizedExperiment objects}
}
\value{
output An HTML tag object that can be rendered as HTML using
  as.character()
}
\description{
This module produces a differential exon usage plot using the \code{plotDEXSeq}
function of the DEXSeq package.
}
\details{
For the plot to be displayed, the \code{dexseq_results} slot must be filled
on at least one of the component \code{ExploratorySummarizedExperiment} objects
of the input \code{ExploratorySummarizedExperimentList}.

\code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
to the contrasts listed in the \code{contrasts} slot of the
\code{ExploratorySummarizedExperiment}.
}
\examples{
dexseqplotInput("experiment", eselist)

}
\keyword{shiny}
