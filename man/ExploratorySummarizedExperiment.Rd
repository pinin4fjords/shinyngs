% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExploratorySummarizedExperiment-class.R
\name{ExploratorySummarizedExperiment}
\alias{ExploratorySummarizedExperiment}
\title{ExploratorySummarizedExperiments}
\usage{
ExploratorySummarizedExperiment(
  assays,
  colData,
  annotation,
  idfield,
  labelfield = character(),
  entrezgenefield = character(),
  contrast_stats = list(),
  assay_measures = list(),
  gene_set_analyses = list(),
  dexseq_results = list(),
  read_reports = list()
)
}
\arguments{
\item{assays}{An object of class SimpleList as would be supplied to the
SummarizedExperiment constructor}

\item{colData}{An object of class DataFrame as would be supplied to the
SummarizedExperimentConstructor. Row names could correspond to column names
in the matrices in \code{assays}}

\item{annotation}{A data frame with annotation for the features (rows) of
the \code{assays} matrices. Rows must correspond to to those matrices.}

\item{idfield}{To which of the \code{annotation} columns do row names
correspond?}

\item{labelfield}{Which column from \code{annotation} should be used to
label features (e.g. a gene name field)?}

\item{entrezgenefield}{Which column from \code{annotation} is the Entrez
gene ID?}

\item{contrast_stats}{List of matrices containing contrast-related
statistics. Only 'pvals', 'qvals' and 'fold_changes' are currently used.
Fold changes are calculated on the fly where not supplied. Matrix columns
correspond to 'contrasts' set in the containing SummarizedExperimentList.}

\item{assay_measures}{Optional List of measures to display related to each
assay.}

\item{gene_set_analyses}{List of lists of gene set tables keyed first by
gene set
type and secondly by contrast}

\item{dexseq_results}{An optional list of \code{DEXSeqResults} objects
corresponding to the contrasts listed in the \code{contrasts} slot..}

\item{read_reports}{A named list of matrices with read counts in columns
and sample names in rows. Useful for providing mapped read counts,
counts per gene type etc}
}
\value{
output An ExploratoryRangedSummarizedExperient object
}
\description{
This function creates objects of the ExploratorySummarizedExperiment class,
an extension of SummarizedExperiment designed to hold additional information
about the features present - for example differential expression values
and the type of identifiers used in rows.
}
\details{
It is intended that one or more ExploratorySummarizedExperiments with the
same samples (columns) are contained within an
ExploratorySumarizedExperimentList, which will contain information relevant
to all experiments such as gene sets and contrasts.

It's clear that the structure of this class and that of SummarizedExperimentList
will need to be refined in future.
}
