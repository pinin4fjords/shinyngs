% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexseqtable.R
\name{dexseqtableOutput}
\alias{dexseqtableOutput}
\title{The output function of the dexseqtable module}
\usage{
dexseqtableOutput(id)
}
\arguments{
\item{id}{Module namespace}
}
\value{
output An HTML tag object that can be rendered as HTML using
as.character()
}
\description{
This module produces a differential exon usage table based on the output
\code{DEXSeqResults} object of the DEXSeq package.
}
\details{
For the table to be displayed, the \code{dexseq_results} slot must be filled
on at least one of the component \code{ExploratorySummarizedExperiment} objects
of the input \code{ExploratorySummarizedExperimentList}.

\code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
to the contrasts listed in the \code{contrasts} slot of the
\code{ExploratorySummarizedExperiment}.

Leverages the \code{simpletable} module
}
\examples{
dexseqtableOutput("experiment")

}
\keyword{shiny}
