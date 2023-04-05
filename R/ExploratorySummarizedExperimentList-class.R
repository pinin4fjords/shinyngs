#' The ExploratorySummaizedExperimentList class
#'
#' @slot .Data list.
#' @slot title character.
#' @slot author character.
#' @slot description character.
#' @slot static_pdf character.
#' @slot group_vars character.
#' @slot default_groupvar character.
#' @slot contrasts list.
#' @slot url_roots list.
#' @slot gene_sets list.
#' @slot gene_set_id_type character.
#' @slot ensembl_species character.
#'
#' @export

setClass("ExploratorySummarizedExperimentList", contains = "list", representation = representation(
  title = "character", author = "character", description = "character", static_pdf = "character",
  group_vars = "character", default_groupvar = "character", contrasts = "list", url_roots = "list", gene_sets = "list", gene_set_id_type = "character", ensembl_species = "character"
))

#' Extract parts of ExploratorySummarizedExperimentList.
#'
#' @param x \code{ExploratorySummarizedExperimentList} object
#' @param i index for the ExploratorySummarizedExperimentList list
#' @param j not used
#' @param drop not used
#' @param ... additional arguments not used here
#' @rdname ExploratorySummarizedExperimentList-class
#' @export
setMethod("[", c("ExploratorySummarizedExperimentList", "ANY", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
  initialize(x, x@.Data[i],
    title = x@title, author = x@author, description = x@description, static_pdf = "character", group_vars = x@group_vars, default_groupvar = x@default_groupvar,
    contrasts = x@contrasts, url_roots = x@url_roots, gene_sets = x@gene_sets, gene_set_id_type = x@gene_set_id_type, ensembl_species = x@ensembl_species
  )
})

#' Extract parts of ExploratorySummarizedExperimentList.
#'
#' @param x \code{ExploratorySummarizedExperimentList} object
#' @param i numeric index for the ExploratorySummarizedExperimentList list
#' @param j not used
#' @param drop not used
#' @param ... additional arguments not used here
#' @rdname ExploratorySummarizedExperimentList-class
#' @export
setMethod("[", c("ExploratorySummarizedExperimentList", "numeric", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
  initialize(x, x@.Data[i],
    title = x@title, author = x@author, description = x@description, static_pdf = x@static_pdf, group_vars = x@group_vars, default_groupvar = x@default_groupvar,
    contrasts = x@contrasts, url_roots = x@url_roots, gene_sets = x@gene_sets, gene_set_id_type = x@gene_set_id_type, ensembl_species = x@ensembl_species
  )
})

#' Extract parts of ExploratorySummarizedExperimentList.
#'
#' @param x \code{ExploratorySummarizedExperimentList} object
#' @param i boolean index for the ExploratorySummarizedExperimentList list
#' @param j not used
#' @param drop not used
#' @param ... additional arguments not used here
#' @rdname ExploratorySummarizedExperimentList-class
#' @export
setMethod("[", c("ExploratorySummarizedExperimentList", "logical", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
  initialize(x, x@.Data[i],
    title = x@title, author = x@author, description = x@description, static_pdf = x@static_pdf, group_vars = x@group_vars, default_groupvar = x@default_groupvar,
    contrasts = x@contrasts, url_roots = x@url_roots, gene_sets = x@gene_sets, gene_set_id_type = x@gene_set_id_type, ensembl_species = x@ensembl_species
  )
})

#' ExploratorySummarizedExperimentLists, containers for
#' ExploratorySummarizedExperiments
#'
#' ExploratorySummarizedExperiment lists are intented to contain one or more
#' ExploratorysummarizedExperiments with the same sets of samples/columns
#' but different feature sets. The motivating use case was the desire to
#' examine expression at both transcript and gene levels in RNA-seq experiments
#' explorted via \code{Shinyngs}
#'
#' As a the containing object for experiments, this class is intented to
#' contain various variables relevant across a whole study, which will be
#' displayed in an exploratory interface generated in \code{shinyngs}. This
#' includes the study title, author etc as well as definitions of the contrasts
#' used in differential analysis and the gene sets relevant to all experiments
#'
#' @param eses List of ExploratorySummarizedExperiments
#' @param title Study title
#' @param author Study authors
#' @param description Study summary to displayed on front page
#' @param static_pdf A URL to a static PDF document to be displayed on the
#' front page.
#' @param group_vars Variables by which a user will be allowed to group the
#' samples of individual experiments, must correspond to their \code{colData}
#' @param default_groupvar Default \code{group_var}
#' @param contrasts List of length-3 vectors containing 1) the
#' \code{group_var}, 2) the \code{group_var} value corresponding to the
#' 'control' side and 3) the value corresponding to the 'treatment' side
#' @param url_roots A list of URL roots, with list names corresponding to
#' metadata column names of the experiments. Exploratory tools displayed via
#' \code{shinyngs} can use these roots to construct URLs to 'link out'.
#' @param gene_sets A named list of GeneSetCollections as might be produced by
#' reading .gmt format gene sets (for example from MSigDB) using
#' GSEABase::getGmt(). These must contain identifiers of the type specified
#' in \code{gene_set_id_type}.
#' @param gene_set_id_type A column found in the metadata of the component
#' ExploratorySummarizedExperiment objects via \code{mcols()}. Used to relate
#' the rows of assays to gene sets.
#' @param ensembl_species Ensembl species definition like 'mmusculus'. Used to
#'   interface with BiomaRt e.g. to make gene model plots in the \code{gene}
#'   module.
#'
#' @return output An ExploratorySummarizedExperimentList
#' @export

ExploratorySummarizedExperimentList <- function(eses, title = "", author = "", description = "", static_pdf = character(), group_vars = character(), default_groupvar = character(),
                                                contrasts = list(), url_roots = list(), gene_sets = list(), gene_set_id_type = character(), ensembl_species = character()) {
  if (!is.list(eses)) {
    eses <- list(eses)
  }

  # Make sure the eses are named for internal purposes

  if (is.null(names(eses))) {
    names(eses) <- "noname"
  }

  # Set grouping variales as any non-integer field applied to more than one sample

  if (length(group_vars) == 0) {
    group_vars <- chooseGroupingVariables(data.frame(colData(eses[[1]]), check.names = FALSE))
    default_groupvar <- group_vars[1]
  }

  # If gene sets are provided, key the gene sets by gene name for easier access

  if (length(gene_sets) > 0 && length(gene_set_id_type) != 0) {
    gene_sets_by_name <- list()

    for (ese in eses) {
      annotation <- data.frame(SummarizedExperiment::mcols(ese))
      labelfield <- ese@labelfield

      if (labelfield %in% names(gene_sets_by_name)) {
        next
      }

      if (!gene_set_id_type %in% colnames(annotation)) {
        next
      } else {
        # Numeric IDs (like entrez will be cast to integers)

        is_numeric <- all(!is.na(as.numeric(annotation[[gene_set_id_type]])))
      }

      print("Processing gene sets")

      gene_sets_by_name[[labelfield]] <- lapply(gene_sets, function(gene_set_collection) {
        # gene_set_collection doesn't behave exactly like a list (it's a GSEABase object), so we have to make sure the result gets named properly

        gsc <- lapply(gene_set_collection, function(gene_set) {
          gene_set_ids <- GSEABase::geneIds(gene_set)
          if (is_numeric) {
            gene_set_ids <- as.integer(gene_set_ids)
          }

          gs <- structure(annotation[match(gene_set_ids, annotation[[gene_set_id_type]]), labelfield], names = gene_set_ids)
          gs[!is.na(gs)]
        })
        names(gsc) <- names(gene_set_collection)
        gsc
      })
      print("Done processing")
    }

    gene_sets <- gene_sets_by_name
  } else {
    gene_sets <- list()
  }

  print("Creating ExploratorySummarizedExperimentList object")

  new("ExploratorySummarizedExperimentList", eses,
    title = title, author = author, description = description, static_pdf = static_pdf, group_vars = group_vars, default_groupvar = default_groupvar,
    contrasts = contrasts, url_roots = url_roots, gene_set_id_type = gene_set_id_type, gene_sets = gene_sets, ensembl_species = ensembl_species
  )
}
