#' Build path to the enrichment results
#'
#' @details
#'
#' The template accepts the following:
#'
#' \describe{
#'   \item{\code{\{contrast_name\}}}{Will be replaced by \code{contrast_info$id} argument}
#'   \item{\code{\{geneset_type\}}}{Will be replaced by the \code{geneset_type} argument}
#'   \item{\code{\{target|reference\}}}{If the \code{direction} argument is \code{"up"}, will be replaced
#'   with \code{contrast_info$target}, if it is \code{"down"}, \code{contrast_info$reference} will be used instead.}
#' }
#'
#' @param template A string, such as \code{"/path/to/folder/{contrast_name}-{geneset_type}.csv"} or
#' \code{"./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv"}
#' @param contrast_info  A list with contrast details: `id`, `reference`, and `target`,
#'   to be replaced in template.
#' @param geneset_type The name of the geneset type, to be replaced in the template
#' @param direction Either `"up"`, `"down"` or `NULL`, used to determine how the replacement will happen.
#'
#' @returns A string similar to template, but with the templates replaced
#' @export
#' @examples
#' build_enrichment_path(
#'   template = "./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv",
#'   contrast_info = list(id = "disease_vs_ctrl", reference = "control", target = "disease"),
#'   geneset_type = "m2.cp.v2024.1.Mm.entrez",
#'   direction = "up"
#' )
#'
build_enrichment_path <- function(template, contrast_info, geneset_type, direction = NULL) {
  path <- template
  path <- gsub("{contrast_name}", contrast_info$id, path, fixed = TRUE)
  path <- gsub("{geneset_type}", geneset_type, path, fixed = TRUE)
  if (!is.null(direction)) {
    target_val <- if (direction == "up") contrast_info$target else contrast_info$reference
    path <- gsub("{target|reference}", target_val, path, fixed = TRUE)
  }
  path
}

#' Detects the enrichment tool used
#'
#' @noRd
#' @param gst The enrichment table
#'
#' @returns The enrichment tool as a string, based on whether "NOM p-val" is a column ("gsea") or
#' either "p value" or "PValue" are found ("roast")
detect_enrichment_tool <- function(gst) {
  if ("NOM p-val" %in% colnames(gst)) {
    return("gsea")
  }
  if (any(c("p value", "PValue") %in% colnames(gst))) {
    return("roast")
  }
  stop("Could not detect enrichment tool from column names")
}



# Column names an enrichment mapping must provide.
enrichment_mapping_fields <- c("pvalue", "fdr", "direction")

# Is gs_tool an explicit column mapping (named pvalue/fdr/direction) rather than
# a built-in tool name? This is what lets tools other than roast/gsea be used:
# the caller names the columns directly.
is_enrichment_mapping <- function(gs_tool) {
  !is.null(names(gs_tool)) && all(enrichment_mapping_fields %in% names(gs_tool))
}

#' Get the expected column names for the gene set enrichment tool
#'
#' @noRd
#' @param gst The enrichment table.
#' @param gs_tool Either `"roast"`, `"gsea"`, or a named vector/list giving the
#' `pvalue`, `fdr` and `direction` column names directly (for other tools).
#'
#' @returns A list with elements `"pvalue"`, `"fdr"` and `"direction"`, each the
#' column name to use for that quantity.
get_enrichment_mapping <- function(gst, gs_tool) {
  if (is_enrichment_mapping(gs_tool)) {
    return(lapply(as.list(gs_tool)[enrichment_mapping_fields], as.character))
  }
  mappings <- list(
    roast = list(pvalue = "p value", fdr = "FDR", direction = "Direction"),
    gsea = list(pvalue = "NOM p-val", fdr = "FDR q-val", direction = "Direction")
  )
  if (identical(gs_tool, "roast") && "PValue" %in% colnames(gst)) {
    mappings[["roast"]][["pvalue"]] <- "PValue"
  }
  if (!is.character(gs_tool) || length(gs_tool) != 1 || !gs_tool %in% names(mappings)) {
    stop(
      "Invalid enrichment tool: ", paste(gs_tool, collapse = ", "),
      ". Use 'gsea', 'roast', or a mapping naming ", paste(enrichment_mapping_fields, collapse = "/"), "."
    )
  }
  mappings[[gs_tool]]
}

#' Friendly label for an enrichment tool/mapping, for display in the app
#' @noRd
#' @param gs_tool Either \code{"gsea"}, \code{"roast"}, or a column mapping (see
#' \code{is_enrichment_mapping()}). Should already be resolved (not \code{"auto"}).
#'
#' @returns A human-readable description of the tool/method that produced an
#' enrichment table.
enrichment_tool_label <- function(gs_tool) {
  if (is_enrichment_mapping(gs_tool)) {
    cols <- as.list(gs_tool)[enrichment_mapping_fields]
    return(sprintf(
      "Custom (p value: %s, FDR: %s, direction: %s)",
      cols$pvalue, cols$fdr, cols$direction
    ))
  }
  switch(gs_tool,
    gsea = "GSEA (Gene Set Enrichment Analysis)",
    roast = "ROAST (rotation gene set test)",
    as.character(gs_tool)
  )
}

# Errors if the table is missing expected columns; returns the column mapping.
validate_enrichment_table <- function(gst, gs_tool) {
  col_map <- get_enrichment_mapping(gst, gs_tool)
  for (col in col_map) {
    if (!col %in% colnames(gst)) {
      stop(paste0(col, " column not found in gst. Found: ", paste0(colnames(gst), collapse = ", ")))
    }
  }
  invisible(col_map)
}

clean_enrichment_table <- function(gst, gs_tool) {
  if (identical(gs_tool, "gsea")) {
    # gsea tsv files have two useless columns that can be removed:
    cols_to_remove <- c("GS<br> follow link to MSigDB", "GS DETAILS")
    gst <- gst[, !(colnames(gst) %in% cols_to_remove), drop = FALSE]
  }
  gst
}

#' Resolve which gene_set_analyses entry corresponds to a selected contrast
#' @noRd
#' @param analyses The gene set analysis entries for a given assay and gene set
#' type (i.e. \code{gene_set_analyses[[assay]][[type]]}), a named list keyed by
#' contrast.
#' @param contrast_number Position of the selected contrast in the containing
#' \code{contrasts} slot.
#' @param contrast The selected contrast record from the \code{contrasts} slot,
#' either a named vector with an \code{id} (and/or \code{Variable}, \code{Group.1},
#' \code{Group.2}) or a bare \code{c(variable, reference, target)} vector.
#'
#' @return The name (character) or position (integer) to index \code{analyses}
#' with, or \code{NULL} when nothing matches. Matching by the contrast identifier
#' is preferred so a stored order that differs from the contrasts order still
#' resolves to the correct entry; otherwise it falls back to the positional index.
#' The same key is valid for the parallel \code{gene_set_analyses_tool} entries,
#' which mirror this structure.
resolve_contrast_key <- function(analyses, contrast_number, contrast) {
  entry_names <- names(analyses)
  candidates <- character(0)
  if (!is.null(names(contrast)) && "id" %in% names(contrast)) {
    candidates <- c(candidates, unname(contrast[["id"]]))
  }
  if (!is.null(names(contrast)) && all(c("Variable", "Group.1", "Group.2") %in% names(contrast))) {
    candidates <- c(candidates, paste(contrast[c("Variable", "Group.1", "Group.2")], collapse = "-"))
  } else if (is.null(names(contrast))) {
    candidates <- c(candidates, paste(contrast, collapse = "-"))
  }
  hit <- candidates[candidates %in% entry_names]
  if (length(hit) >= 1) {
    return(hit[1])
  }
  if (!is.na(contrast_number) && contrast_number >= 1 && contrast_number <= length(analyses)) {
    return(contrast_number)
  }
  NULL
}

# Resolve gs_tool to a concrete tool name, auto-detecting from gst's columns
# when unset or "auto". Shared by resolve_enrichment() and
# check_gene_set_analyses_tool_consistency().
resolve_gene_set_analyses_tool <- function(gst, gs_tool) {
  if (is.null(gs_tool) || identical(gs_tool, "auto")) {
    return(detect_enrichment_tool(gst))
  }
  gs_tool
}

#' Resolve the cleaned enrichment table and column mapping for a contrast
#' @noRd
#' @param ese An ExploratorySummarizedExperiment.
#' @param assay,gene_set_type Keys into \code{ese@gene_set_analyses}.
#' @param contrast_number Position of the selected contrast in \code{contrasts}.
#' @param contrast The selected contrast record (see \code{resolve_contrast_key}).
#'
#' @return \code{NULL} when there is no usable result for the selection,
#' otherwise a list with \code{gst} (the cleaned enrichment table), \code{col_map}
#' (its pvalue/fdr/direction column names) and \code{tool} (the resolved tool,
#' e.g. \code{"gsea"}, \code{"roast"} or a column mapping - never \code{"auto"}).
#' The enrichment tool is taken from the \code{gene_set_analyses_tool} slot when
#' present (older objects predating the slot fall back to auto-detection).
resolve_enrichment <- function(ese, assay, gene_set_type, contrast_number, contrast) {
  analyses <- ese@gene_set_analyses[[assay]][[gene_set_type]]
  contrast_key <- resolve_contrast_key(analyses, contrast_number, contrast)
  if (is.null(contrast_key)) {
    return(NULL)
  }
  gst <- analyses[[contrast_key]]
  if (is.null(gst) || nrow(gst) == 0) {
    return(NULL)
  }

  gs_tool <- if (.hasSlot(ese, "gene_set_analyses_tool")) {
    ese@gene_set_analyses_tool[[assay]][[gene_set_type]][[contrast_key]]
  } else {
    "auto"
  }
  gs_tool <- resolve_gene_set_analyses_tool(gst, gs_tool)
  col_map <- validate_enrichment_table(gst, gs_tool)
  list(gst = clean_enrichment_table(gst, gs_tool), col_map = col_map, tool = gs_tool)
}
