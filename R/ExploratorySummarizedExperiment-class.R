#' The ExploratorySummarizedExperiment class
#' 
#' Subclass of SummarizedExperiment if present in the SummarizedExperiment
#' package (newer versions of Bioconductor have moved this from GenomicRanges), 
#' otherwise of SummarizedExperiment0.
#'
#' @slot idfield character. 
#' @slot entrezgenefield character. 
#' @slot labelfield character. 
#' @slot tests 
#' @slot read_reports list.
#' 
#' @export

setClass("ExploratorySummarizedExperiment", contains = ifelse("SummarizedExperiment" %in% getClasses(where = "package:SummarizedExperiment"), "SummarizedExperiment", 
    "SummarizedExperiment0"), representation = representation(idfield = "character", entrezgenefield = "character", labelfield = "character", tests = "list", 
    assay_measures = "list", gene_set_analyses = "list", dexseq_results = "list", read_reports = "list"))

setAs("RangedSummarizedExperiment", "ExploratorySummarizedExperiment", function(from) {
    as((as(from, ifelse("SummarizedExperiment" %in% getClasses(where = "package:SummarizedExperiment"), "SummarizedExperiment", "SummarizedExperiment0"))), 
        "ExploratorySummarizedExperiment")
})

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
#' gene ID? 
#' @param tests List of matrices keyed by 'pvals' and 'qvals', with columns
#' corresponding to 'contrasts' set in the containing SummarizedExperimentList
#' @param gene_set_analyses List of lists of gene set tables keyed first by gene set
#' type and secondly by contrast
#' @param read_reports A named list of matrices with read counts in columns
#' and sample names in rows. Useful for providing mapped read counts, 
#' counts per gene type etc
#'
#' @return output An ExploratoryRangedSummarizedExperient object
#' @import SummarizedExperiment
#' @export

ExploratorySummarizedExperiment <- function(assays, colData, annotation, idfield, labelfield = character(), entrezgenefield = character(), tests = list(), 
    assay_measures = list(), gene_set_analyses = list(), dexseq_results = list(), read_reports = list()) {

    # Reset NULLs to empty 
  
    if (is.null(entrezgenefield)){
      entrezgenefield <- character()
    }
  
    # The assays slot of a summarised experiment needs the same dimensions for every matrix
    
    all_rows <- Reduce(union, lapply(assays, rownames))
    add_missing_rows <- function(x){
      missing_rows <- all_rows[! all_rows %in% rownames(x)]
      empty_rows <- data.frame(matrix(NA, nrow = length(missing_rows), ncol = ncol(x)), row.names = missing_rows)
      colnames(empty_rows) <- colnames(x)
      rbind(x, empty_rows)[all_rows, , drop = FALSE]
    }

    # Subset colData to remove any samples not present in the first assay
    
    colData <- colData[rownames(colData) %in% colnames(assays[[1]]),]
    
    assays <- SimpleList(lapply(assays, function(as){
      round(add_missing_rows(as)[, rownames(colData)], 2)
    }))  
    
    # The same fix for tests
    
    if (length(tests) > 0){
      
      tests <- lapply(tests, function(stats){
        lapply(stats, function(test){
          add_missing_rows(test)
        })
      })
    }
    
    # Annotations need to be strings
    
    annotation <- data.frame(lapply(annotation, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    
    # Build the object
    
    sumexp <- SummarizedExperiment(assays = assays, colData = DataFrame(colData))
    mcols(sumexp) <- annotation
    
    new("ExploratorySummarizedExperiment", sumexp, idfield = idfield, labelfield = labelfield, entrezgenefield = entrezgenefield, assay_measures = assay_measures, 
        tests = tests, gene_set_analyses = gene_set_analyses, dexseq_results = dexseq_results, read_reports = read_reports)
} 
