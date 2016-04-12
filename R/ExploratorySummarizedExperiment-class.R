#' The ExploratorySummarizedExperiment class
#'
#' @slot idfield character. 
#' @slot entrezgenefield character. 
#' @slot labelfield character. 
#' @slot tests 
#' 
#' @export

setClass("ExploratorySummarizedExperiment", contains = "SummarizedExperiment0", representation = representation(idfield = "character", entrezgenefield = "character", labelfield = "character", 
    tests = "list"))

#' ExploratorySummarizedExperiments
#' 
#' This function creates objects of the ExploratorySummarizedExperiment class,
#' an extension of SummarizedExperiment designed to hold additional information
#' about the features present - for example differential expression values
#' and the type of identifiers used in rows. 
#' 
#' It is intended that one or more ExploratorySummarizedExperiments with the
#' same samples (columns) are contained within an 
#' ExploratorySumarizedExperimentList, which will contain information relevant
#' to all experiments such as gene sets and contrasts. 
#' 
#' It's clear that the structure of this class and that of SummarizedExperimentList
#' will need to be refined in future. 
#'
#' @param assays An object of class SimpleList as would be supplied to the 
#' SummarizedExperiment constructor
#' @param colData An object of class DataFrame as would be supplied to the
#' SummarizedExperimentConstructor. Row names could correspond to column names
#' in the matrices in \code{assays}
#' @param annotation A data frame with annotation for the features (rows) of
#' the \code{assays} matrices. Rows must correspond to to those matrices.
#' @param idfield To which of the \code{annotation} columns do row names
#' correspond?
#' @param labelfield Which column from \code{annotation} should be used to
#' label features (e.g. a gene name field)? 
#' @param entrezgenefield Which column from \code{annotation} is the Entrez 
#' gene ID? Important mostly for gene sets where members are commonly indicated
#' by that identifier type.
#' @param tests List of matrices keyed by 'pvals' and 'qvals', with columns
#' corresponding to 'contrasts' set in the containing SummarizedExperimentList
#'
#' @return output An ExploratoryRangedSummarizedExperient object
#' @import SummarizedExperiment
#' @export

ExploratorySummarizedExperiment <- function(assays, colData, annotation, idfield, labelfield = character(), entrezgenefield = character(), tests = list()) {
    
    sumexp <- SummarizedExperiment(assays = assays, colData = DataFrame(colData))
    mcols(sumexp) <- annotation
    
    new("ExploratorySummarizedExperiment", sumexp, idfield = idfield, labelfield = labelfield, entrezgenefield = entrezgenefield, tests = tests)
} 
