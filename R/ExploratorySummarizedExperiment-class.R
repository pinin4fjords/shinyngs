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
#'
#' @examples
#' expression <- matrix(1:12, nrow = 3,
#'   dimnames = list(c("ENSG1", "ENSG2", "ENSG3"), paste0("s", 1:4)))
#' coldata <- data.frame(
#'   condition = rep(c("treated", "control"), each = 2),
#'   row.names = paste0("s", 1:4)
#' )
#' annotation <- data.frame(
#'   gene_id = c("ENSG1", "ENSG2", "ENSG3"),
#'   gene_name = c("GeneA", "GeneB", "GeneC"),
#'   row.names = c("ENSG1", "ENSG2", "ENSG3")
#' )
#' ExploratorySummarizedExperiment(
#'   assays = list(expression = expression),
#'   colData = coldata,
#'   annotation = annotation,
#'   idfield = "gene_id",
#'   labelfield = "gene_name"
#' )
#'
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

  colData <- colData[rownames(colData) %in% colnames(assays[[1]]), , drop = FALSE]

  assays <- SimpleList(lapply(assays, function(as) {
    round(add_missing_rows(as)[, rownames(colData), drop = FALSE], 2)
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

  annotation <- data.frame(lapply(annotation, as.character), check.names = FALSE, row.names = rownames(annotation))[all_rows, ]

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
#' @noRd
#'
#' @description
#' Ensures that the structure of \code{gene_set_analyses_tool} matches that of \code{gene_set_analyses},
#' filling in missing elements as needed. Each entry in \code{gene_set_analyses_tool} should be a string
#' (e.g., "auto", "gsea", or "roast") corresponding to the format of the associated gene set analysis table.
#' Every non-NULL table in \code{gene_set_analyses} is also validated against its resolved tool's expected
#' columns (auto-detecting the tool first where the entry is "auto"), so malformed enrichment tables are
#' rejected here rather than only when rendered in the app.
#'
#' @param gene_set_analyses A three-level nested list of gene set tables, keyed by assay, gene set type, and contrast.
#' @param gene_set_analyses_tool A three-level nested list of strings, structured as \code{gene_set_analyses}, indicating the tool used for each gene set analysis.
#'
#' @return A three-level nested list of strings, matching the structure of \code{gene_set_analyses}, with missing elements filled as needed.
check_gene_set_analyses_tool_consistency <- function(gene_set_analyses, gene_set_analyses_tool) {
  if (is.null(gene_set_analyses_tool)) {
    gene_set_analyses_tool <- list()
  }

  validate_tool <- function(tool, where) {
    ok <- (is.character(tool) && length(tool) == 1 && tool %in% c("auto", "gsea", "roast")) ||
      is_enrichment_mapping(tool)
    if (!ok) {
      stop(
        "Invalid gene_set_analyses_tool for ", where,
        ". Expected 'auto', 'gsea', 'roast', or a named vector giving ",
        paste(enrichment_mapping_fields, collapse = "/"), " columns. Found ",
        paste(tool, collapse = ",")
      )
    }
    tool
  }

  # gene_set_analyses is the source of truth for structure; mirror the tool spec onto it.
  mirror <- function(analyses, tools, path) {
    lapply(setNames(nm = names(analyses)), function(name) {
      node <- analyses[[name]]
      tool_node <- if (is.null(tools)) NULL else tools[[name]]
      if (is.list(node) && !is.data.frame(node)) {
        mirror(node, tool_node, c(path, name)) # assay / gene-set-type container
      } else {
        where <- paste(c(path, name), collapse = "/")
        resolved_tool <- validate_tool(
          if (is.null(tool_node)) "auto" else tool_node,
          where
        ) # contrast leaf (table or NULL)

        if (!is.null(node)) {
          actual_tool <- tryCatch(
            resolve_gene_set_analyses_tool(node, resolved_tool),
            error = function(e) stop("Could not detect gene_set_analyses_tool for ", where, ": ", conditionMessage(e))
          )
          tryCatch(
            validate_enrichment_table(node, actual_tool),
            error = function(e) stop("Invalid gene_set_analyses table for ", where, ": ", conditionMessage(e))
          )
        }

        resolved_tool
      }
    })
  }

  mirror(gene_set_analyses, gene_set_analyses_tool, character(0))
}
