% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexseqplot.R
\name{dexseqplot}
\alias{dexseqplot}
\title{The server function of the dexseqplot Shiny module}
\usage{
dexseqplot(input, output, session, eselist)
}
\arguments{
\item{input}{Input object}

\item{output}{Output object}

\item{session}{Session object}

\item{eselist}{ExploratorySummarizedExperimentList object containing
ExploratorySummarizedExperiment objects}
}
\description{
This function is not called directly, but rather via callModule() (see
example).
}
\details{
This module produces a differential exon usage plot using the \code{plotDEXSeq}
function of the DEXSeq package.

For the plot to be displayed, the \code{dexseq_results} slot must be filled
on at least one of the component \code{ExploratorySummarizedExperiment} objects
of the input \code{ExploratorySummarizedExperimentList}.

\code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
to the contrasts listed in the \code{contrasts} slot of the
\code{ExploratorySummarizedExperiment}.
}
\examples{
callModule(dexseqplot, "dexseqplot", eselist)

}
\keyword{shiny}
