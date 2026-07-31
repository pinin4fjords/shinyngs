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

setValidity("ExploratorySummarizedExperiment", function(object) {
  errors <- character()
  annotation_fields <- colnames(SummarizedExperiment::mcols(object))

  validate_annotation_field <- function(field, slot_name, required = FALSE) {
    if ((required && length(field) != 1) || (!required && length(field) > 1)) {
      errors <<- c(errors, paste0(slot_name, " must contain ", if (required) "one field" else "at most one field"))
    } else if (length(field) == 1 && !field %in% annotation_fields) {
      errors <<- c(errors, paste0(slot_name, " field '", field, "' is absent from the feature annotation"))
    }
  }

  validate_annotation_field(object@idfield, "idfield")
  validate_annotation_field(object@labelfield, "labelfield")
  validate_annotation_field(object@entrezgenefield, "entrezgenefield")

  if (length(object@assay_measures) > 0 && (is.null(names(object@assay_measures)) || any(!names(object@assay_measures) %in% SummarizedExperiment::assayNames(object)))) {
    errors <- c(errors, "assay_measures must be named for assays in the object")
  }

  if (length(errors) == 0) TRUE else errors
})

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
#' @param assay_digits Number of decimal places retained in assays. The default
#' reduces compressed serialized object size. Use \code{NULL} to preserve full
#' numeric precision.
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
                                            assay_measures = list(), assay_digits = 2, gene_set_analyses = list(), dexseq_results = list(), read_reports = list(), gene_set_analyses_tool = list()) {
  # Reset NULLs to empty

  if (is.null(entrezgenefield)) {
    entrezgenefield <- character()
  }

  if ((!is.list(assays) && !methods::is(assays, "List")) || length(assays) == 0) {
    stop("assays must be a non-empty list")
  }
  if (!is.null(assay_digits) && (!is.numeric(assay_digits) || length(assay_digits) != 1 || is.na(assay_digits) || assay_digits < 0 || assay_digits != as.integer(assay_digits))) {
    stop("assay_digits must be NULL or one non-negative integer")
  }
  if (is.null(rownames(colData)) || anyDuplicated(rownames(colData))) {
    stop("colData must have unique sample row names")
  }

  assay_errors <- vapply(seq_along(assays), function(index) {
    assay <- assays[[index]]
    assay_name <- if (!is.null(names(assays)) && nzchar(names(assays)[index])) names(assays)[index] else index
    if (length(dim(assay)) != 2) {
      return(paste0("assay '", assay_name, "' is not two-dimensional"))
    }
    if (!is.numeric(assay)) {
      return(paste0("assay '", assay_name, "' must contain numeric values"))
    }
    if (is.null(rownames(assay)) || anyDuplicated(rownames(assay))) {
      return(paste0("assay '", assay_name, "' must have unique feature row names"))
    }
    if (is.null(colnames(assay)) || anyDuplicated(colnames(assay))) {
      return(paste0("assay '", assay_name, "' must have unique sample column names"))
    }
    ""
  }, character(1))
  assay_errors <- assay_errors[nzchar(assay_errors)]
  if (length(assay_errors) > 0) {
    stop(paste(assay_errors, collapse = "; "))
  }

  # The assays slot of a summarised experiment needs the same dimensions for every matrix

  all_rows <- Reduce(union, lapply(assays, rownames))
  add_missing_rows <- function(x) {
    missing_rows <- all_rows[!all_rows %in% rownames(x)]
    empty_rows <- data.frame(matrix(NA, nrow = length(missing_rows), ncol = ncol(x)), row.names = missing_rows)
    colnames(empty_rows) <- colnames(x)
    rbind(x, empty_rows)[all_rows, , drop = FALSE]
  }

  # The first assay defines which sample metadata rows belong to this object.

  missing_from_first <- setdiff(rownames(colData), colnames(assays[[1]]))
  if (length(missing_from_first) > 0) {
    warning("Dropping colData samples absent from the first assay: ", paste(missing_from_first, collapse = ", "))
  }
  colData <- colData[rownames(colData) %in% colnames(assays[[1]]), , drop = FALSE]
  if (nrow(colData) == 0) {
    stop("No colData samples are present in the first assay")
  }
  missing_by_assay <- lapply(assays, function(assay) setdiff(rownames(colData), colnames(assay)))
  if (any(lengths(missing_by_assay) > 0)) {
    details <- vapply(which(lengths(missing_by_assay) > 0), function(index) {
      assay_name <- if (!is.null(names(assays)) && nzchar(names(assays)[index])) names(assays)[index] else index
      paste0("assay '", assay_name, "': ", paste(missing_by_assay[[index]], collapse = ", "))
    }, character(1))
    stop("Samples are missing from assays (", paste(details, collapse = "; "), ")")
  }

  assays <- SimpleList(lapply(assays, function(as) {
    aligned_assay <- add_missing_rows(as)[, rownames(colData), drop = FALSE]
    if (is.null(assay_digits)) aligned_assay else round(aligned_assay, assay_digits)
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

  if (is.null(rownames(annotation)) || anyDuplicated(rownames(annotation))) {
    stop("annotation must have unique feature row names")
  }
  missing_annotation <- setdiff(all_rows, rownames(annotation))
  if (length(missing_annotation) > 0) {
    stop("Feature annotation is missing assay rows: ", paste(missing_annotation, collapse = ", "))
  }
  annotation <- data.frame(lapply(annotation, as.character), check.names = FALSE, row.names = rownames(annotation))[all_rows, , drop = FALSE]
  annotation_fields <- colnames(annotation)
  validate_field <- function(field, field_name, required = FALSE) {
    if ((required && length(field) != 1) || (!required && length(field) > 1)) {
      stop(field_name, " must contain ", if (required) "one field" else "at most one field")
    }
    if (length(field) == 1 && !field %in% annotation_fields) {
      stop(field_name, " field '", field, "' is absent from the feature annotation")
    }
  }
  validate_field(idfield, "idfield", required = TRUE)
  validate_field(labelfield, "labelfield")
  validate_field(entrezgenefield, "entrezgenefield")

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
