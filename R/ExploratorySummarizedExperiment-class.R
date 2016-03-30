#' Title
#'
#' @slot idfield character. 
#' @slot entrezgenefield character. 
#' @slot labelfield character. 
#' @slot tests 
#' 
#' @export

setClass("ExploratorySummarizedExperiment", contains = "SummarizedExperiment0", representation = representation(idfield = "character", entrezgenefield = "character", labelfield = "character", 
    tests = "list"))

#' Title
#'
#' @param assays 
#' @param colData 
#' @param annotation 
#' @param idfield 
#' @param labelfield 
#' @param entrezgenefield 
#' @param group_vars 
#' @param default_groupvar 
#' @param contrasts 
#' @param url_roots 
#' @param geneset_files 
#' @param tests 
#'
#' @return output An ExploratoryRangedSummarizedExperient object
#' @import SummarizedExperiment
#' @export

ExploratorySummarizedExperiment <- function(assays, colData, annotation, idfield, labelfield = character(), entrezgenefield = character(), tests = list()) {
    
    sumexp <- SummarizedExperiment(assays = assays, colData = DataFrame(colData))
    mcols(sumexp) <- annotation
    
    new("ExploratorySummarizedExperiment", sumexp, idfield = idfield, labelfield = labelfield, entrezgenefield = entrezgenefield, tests = tests)
} 
