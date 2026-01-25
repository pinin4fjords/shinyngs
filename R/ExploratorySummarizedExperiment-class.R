#' The ExploratorySummarizedExperiment class
#'
#' Subclass of SummarizedExperiment.
#'
#' @slot idfield character.
#' @slot entrezgenefield character.
#' @slot labelfield character.
#' @slot contrast_stats list.
#' @slot assay_measures list.
#' @slot gene_set_analyses list.
#' @slot dexseq_results list.
#' @slot read_reports list.
#' @slot gene_set_analyses_tool list.
#'
#' @export

setClass("ExploratorySummarizedExperiment", contains = "SummarizedExperiment", slots = c(
  idfield = "character", entrezgenefield = "character", labelfield = "character", contrast_stats = "list",
  assay_measures = "list", gene_set_analyses = "list", dexseq_results = "list", read_reports = "list", gene_set_analyses_tool = "list"
))

setAs("RangedSummarizedExperiment", "ExploratorySummarizedExperiment", function(from) {
  as(as(from, "SummarizedExperiment"), "ExploratorySummarizedExperiment")
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
#' @param contrast_stats List of matrices containing contrast-related
#' statistics. Only 'pvals', 'qvals' and 'fold_changes' are currently used.
#' Fold changes are calculated on the fly where not supplied. Matrix columns
#' correspond to 'contrasts' set in the containing SummarizedExperimentList.
#' @param assay_measures Optional List of measures to display related to each
#' assay.
#' @param gene_set_analyses Three-level nested lists of gene set tables keyed first by
#' assay, then by gene set type and then by contrast.
#' @param read_reports A named list of matrices with read counts in columns
#' and sample names in rows. Useful for providing mapped read counts,
#' counts per gene type etc
#' @param dexseq_results An optional list of \code{DEXSeqResults} objects
#' corresponding to the contrasts listed in the \code{contrasts} slot..
#' @param gene_set_analyses_tool Three-level nested lists of a string, nested as \code{gene_set_analyses}.
#' Each string may be \code{"auto"} (the default), \code{"gsea"} or \code{"roast"}. It defines the format of the
#' corresponding \code{gene_set_analyses} table.
#'
#' @return output An ExploratoryRangedSummarizedExperient object
#' @rawNamespace import(SummarizedExperiment, except = 'shift')
#' @export

ExploratorySummarizedExperiment <- function(assays, colData, annotation, idfield, labelfield = character(), entrezgenefield = character(), contrast_stats = list(),
                                            assay_measures = list(), gene_set_analyses = list(), dexseq_results = list(), read_reports = list(), gene_set_analyses_tool = list()) {
  # Reset NULLs to empty

  if (is.null(entrezgenefield)) {
    entrezgenefield <- character()
  }

  # The assays slot of a summarised experiment needs the same dimensions for every matrix

  all_rows <- Reduce(union, lapply(assays, rownames))
  add_missing_rows <- function(x) {
    missing_rows <- all_rows[!all_rows %in% rownames(x)]
    empty_rows <- data.frame(matrix(NA, nrow = length(missing_rows), ncol = ncol(x)), row.names = missing_rows)
    colnames(empty_rows) <- colnames(x)
    rbind(x, empty_rows)[all_rows, , drop = FALSE]
  }

  # Subset colData to remove any samples not present in the first assay

  colData <- colData[rownames(colData) %in% colnames(assays[[1]]), ]

  assays <- SimpleList(lapply(assays, function(as) {
    round(add_missing_rows(as)[, rownames(colData)], 2)
  }))

  # The same fix for contrast_stats

  if (length(contrast_stats) > 0) {
    contrast_stats <- lapply(contrast_stats, function(stats) {
      lapply(stats, function(test) {
        add_missing_rows(test)
      })
    })
  }

  # Annotations need to be strings

  annotation <- data.frame(lapply(annotation, as.character), stringsAsFactors = FALSE, check.names = FALSE, row.names = rownames(annotation))[all_rows, ]

  # Ensure consistency between gene_set_analyses with gene_set_analyses_tool
  gene_set_analyses_tool <- check_gene_set_analyses_tool_consistency(gene_set_analyses, gene_set_analyses_tool)
  
  # Build the object

  sumexp <- SummarizedExperiment(assays = assays, colData = DataFrame(colData, check.names = FALSE))
  mcols(sumexp) <- annotation

  new("ExploratorySummarizedExperiment", sumexp,
    idfield = idfield, labelfield = labelfield, entrezgenefield = entrezgenefield, assay_measures = assay_measures,
    contrast_stats = contrast_stats, gene_set_analyses = gene_set_analyses, dexseq_results = dexseq_results, read_reports = read_reports,
    gene_set_analyses_tool = gene_set_analyses_tool
  )
}

#' Ensure consistency between gene_set_analyses and gene_set_analyses_tool structures
#'
#' @description
#' Ensures that the structure of \code{gene_set_analyses_tool} matches that of \code{gene_set_analyses},
#' filling in missing elements as needed. Each entry in \code{gene_set_analyses_tool} should be a string
#' (e.g., "auto", "gsea", or "roast") corresponding to the format of the associated gene set analysis table.
#'
#' @param gene_set_analyses A three-level nested list of gene set tables, keyed by assay, gene set type, and contrast.
#' @param gene_set_analyses_tool A three-level nested list of strings, structured as \code{gene_set_analyses}, indicating the tool used for each gene set analysis.
#'
#' @return A three-level nested list of strings, matching the structure of \code{gene_set_analyses}, with missing elements filled as needed.
check_gene_set_analyses_tool_consistency <- function(gene_set_analyses, gene_set_analyses_tool) {
  # gene_set_analyses and gene_set_analyses_tool should have the same list of lists
  # structure. gene_set_analyses_tool should have a single string

  if (is.null(gene_set_analyses_tool)) {
    gene_set_analyses_tool <- list()
  }
  
  valid_tools <- c("auto", "gsea", "roast")
  
  safe_get <- function(x, path, default="auto") {
    # similar to purrr::pluck()
    if (is.null(x)) {
      return(default)
    }
    for (p in path) {
      if (!p %in% names(x)) {
        return(default)
      }
      x <- x[[p]]
      if (is.null(x)) {
        return(default)
      }
    }
    x
  }

  lapply(
    setNames(nm=names(gene_set_analyses)),
    function(assay_name) {
      gene_sets <- gene_set_analyses[[assay_name]]
      lapply(
        setNames(nm=names(gene_sets)),
        function(gene_set_name) {
          contrasts <- gene_sets[[gene_set_name]]
          lapply(
            setNames(nm=names(contrasts)),
            function(contrast_name) {
              tool_name <- safe_get(gene_set_analyses_tool, c(assay_name, gene_set_name, contrast_name), "auto")
              if (!is.character(tool_name) || length(tool_name) > 1 || !tool_name %in% valid_tools) {
                stop(
                  "Invalid gene_set_analyses_tool. gene_set_analyses_tool for ",
                  assay_name, ",", gene_set_name, ",", contrast_name,
                  ". It should be one of 'auto', 'gsea' or 'roast'. Found ",
                  tool_name
                )
              }
              tool_name
            }
          )
        }
      )
    }
  )
}
