genesetanalysistable_modal <- list(id = "genesetanalysistable", title = "Gene set analysis")

#' The UI input function of the genesetanalysistable module
#'
#' This module displays gene set analysis tables stored as a list in the
#' \code{gene_set_analyses} slot of an \code{ExploratorySummarizedExperiment}.
#'
#' The \code{gene_set_analyses} slot must be keyed first by the name of the
#' assay to which it pertains, and second by the gene set type (e.g. 'KEGG').
#' The containing \code{ExploratorySummarizedExperiment} must have a populated
#' \code{gene_sets} slot, keyed first by metadata column used to define the
#' gene sets and secondly by the gene set type.
#'
#' The module is based on the output of roast() from \code{limma}, but it's
#' fairly generic, and assumes only the presence of a 'p value' and 'FDR'
#' column, so the output of other methods should be easily adapted to suit.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' # This module needs an eselist whose experiments carry gene_set_analyses
#' # results (see the vignette). Given those, module input is produced like:
#'
#' if (interactive()) {
#'   genesetanalysistableInput("experiment", eselist)
#' }
#'
genesetanalysistableInput <- function(id, eselist) {
  ns <- NS(id)

  # Only use experiments with gene set analyses available

  eselist <- eselist[unlist(lapply(eselist, function(ese) has_slot_data(ese, "gene_set_analyses")))]

  # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay select doesn't have invalid options.

  for (exp in names(eselist)) {
    assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
  }

  expression_filters <- selectmatrixInput(ns("expression"), eselist)

  field_sets <- list(gene_set_types = list(uiOutput(ns("geneSets")), uiOutput(ns("geneSetsStatus"))), differential_gene_sets = list(
    numericInput(ns("pval"), "Maximum p value", value = 0.05),
    numericInput(ns("fdr"), "Maximum FDR", value = 0.1)
  ), differential_genes = list(
    helpText("How should significant genes be selected for each set? Note: genes will be restricted to the direction of change assigned to the set."),
    contrastsInput(ns("genesetanalysistable"))
  ))

  # Things we don't want to wrap in a field set - probably hidden stuff

  naked_fields <- list()

  if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
    field_sets$select_assay_data <- expression_filters
  } else {
    naked_fields <- push_to_list(naked_fields, expression_filters)
  }

  field_sets <- c(field_sets, list(export = simpletableInput(ns("genesetanalysistable"), "Gene set analysis")))

  list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The output function of the genesetanalysistable module
#'
#' This module displays gene set analysis tables stored as a list in the
#' \code{gene_set_analyses} slot of an \code{ExploratorySummarizedExperiment}.
#'
#' The \code{gene_set_analyses} slot must be keyed first by the name of the
#' assay to which it pertains, and second by the gene set type (e.g. 'KEGG').
#' The containing \code{ExploratorySummarizedExperiment} must have a populated
#' \code{gene_sets} slot, keyed first by metadata column used to define the
#' gene sets and secondly by the gene set type.
#'
#' The module is based on the output of roast() from \code{limma}, but it's
#' fairly generic, and assumes only the presence of a 'p value' and 'FDR'
#' column, so the output of other methods should be easily adapted to suit.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#'
#' # The module output function is called like:
#'
#' genesetanalysistableOutput("experiment")
#'
genesetanalysistableOutput <- function(id) {
  ns <- NS(id)

  moduleMain(
    "Gene set analysis",
    uiOutput(ns("enrichmentMethod")),
    simpletableOutput(ns("genesetanalysistable"), spinner = TRUE),
    help = modalInput(ns(genesetanalysistable_modal$id), "help", "help")
  )
}

#' The server function of the genesetanalysistable module
#'
#' This module displays gene set analysis tables stored as a list in the
#' \code{gene_set_analyses} slot of an \code{ExploratorySummarizedExperiment}.
#'
#' The \code{gene_set_analyses} slot must be keyed first by the name of the
#' assay to which it pertains, and second by the gene set type (e.g. 'KEGG').
#' The containing \code{ExploratorySummarizedExperiment} must have a populated
#' \code{gene_sets} slot, keyed first by metadata column used to define the
#' gene sets and secondly by the gene set type.
#'
#' The module is based on the output of roast() from \code{limma}, but it's
#' fairly generic, and assumes only the presence of a 'p value' and 'FDR'
#' column, so the output of other methods should be easily adapted to suit.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example). Essentially this just passes the results of \code{colData()}
#' applied to the specified SummarizedExperiment object to the
#' \code{simpletable} module
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' # This module needs an eselist whose experiments carry gene_set_analyses
#' # results (see the vignette). Given those, the module server is called like:
#'
#' if (interactive()) {
#'   genesetanalysistable("genesetanalysistable", eselist)
#' }
#'
genesetanalysistable <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(genesetanalysistable_modal$id, genesetanalysistable_modal$title)

    # Only use experiments with gene set analyses available

    eselist <- eselist[unlist(lapply(eselist, function(ese) has_slot_data(ese, "gene_set_analyses")))]

    # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay select doesn't have invalid options.

    for (exp in names(eselist)) {
      assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
    }

    # Extract the gene sets that have been analysed for the the user to select from

    ns <- session$ns

    output$geneSets <- renderUI({
      genesetselectInput(ns("genesetanalysistable"))
    })

    # Call the selectmatrix module and hold on to the reactives it sends back

    selectmatrix_reactives <- selectmatrix("expression", eselist, select_assays = TRUE, select_samples = FALSE, select_genes = FALSE, select_meta = FALSE)

    # Pass the matrix to the contrasts module for processing

    contrast_reactives <- contrasts("genesetanalysistable", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE, default_foldchange = 1, default_pval = 0.05, default_qval = 1)

    # Parse the gene sets for ease of use

    genesetselect_reactives <- genesetselect("genesetanalysistable", eselist, selectmatrix_reactives$getExperiment, filter_by_type = TRUE, require_select = FALSE)

    # Resolve the enrichment table, column mapping and tool once per selection,
    # shared between the table itself, the method label above it, and the
    # gene set choices offered below.

    getEnrichmentInfo <- reactive({
      ese <- selectmatrix_reactives$getExperiment()
      assay <- selectmatrix_reactives$getAssay()
      gene_set_types <- genesetselect_reactives$getGeneSetTypes()
      contrast_number <- as.numeric(contrast_reactives$getSelectedContrastNumbers()[[1]])

      resolve_enrichment(ese, assay, gene_set_types, contrast_number, eselist@contrasts[[contrast_number]])
    })

    # Which gene set IDs actually have a result row for the current
    # assay/type/contrast selection AND pass the current p value/FDR filters -
    # anything else could only ever resolve to "No results matching specified
    # filters" in the table below. NULL means "no enrichment resolved at all"
    # (nothing to filter down from); character(0) means "resolved, but the
    # filters exclude every set" - the two are distinguished so the message
    # below only appears in the latter case.

    getFilterablePassingIds <- reactive({
      enrichment <- getEnrichmentInfo()
      if (is.null(enrichment) || is.null(input$pval) || is.null(input$fdr)) {
        return(NULL)
      }

      passing_gene_set_ids(enrichment$gst, enrichment$col_map, input$pval, input$fdr)
    })

    # Only push a selectize update when the passing set actually changed -
    # editing the p value/FDR fields invalidates getFilterablePassingIds() on
    # every keystroke, but most edits don't change which gene sets pass.

    lastPassingIds <- reactiveVal(NULL)

    observe({
      passing <- getFilterablePassingIds()
      if (!identical(passing, isolate(lastPassingIds()))) {
        lastPassingIds(passing)
        genesetselect_reactives$updateGeneSetsList(passing)
      }
    })

    output$geneSetsStatus <- renderUI({
      passing <- getFilterablePassingIds()
      validate(need(is.null(passing) || length(passing) > 0, "No gene sets meet the current p value/FDR filters"))
      NULL
    })

    output$enrichmentMethod <- renderUI({
      enrichment <- getEnrichmentInfo()
      if (is.null(enrichment)) {
        return(NULL)
      }
      helpText(paste0("Method: ", enrichment_tool_label(enrichment$tool)))
    })

    getGeneSetAnalysis <- reactive({
      validate(need(input$pval, "Waiting for p value"), need(input$fdr, "Waiting for FDR value"))

      ese <- selectmatrix_reactives$getExperiment()
      gene_set_types <- genesetselect_reactives$getGeneSetTypes()

      enrichment <- getEnrichmentInfo()
      validate(need(!is.null(enrichment), "No enrichment results for this contrast"))
      gst <- enrichment$gst
      col_map <- enrichment$col_map

      # Select out specific gene sets if they've been provided

      selected_gene_sets <- genesetselect_reactives$getGenesetNames()
      if (!is.null(selected_gene_sets)) {
        validate(need(any(selected_gene_sets %in% rownames(gst)), "Selected gene sets not available in test results"))
        gst <- gst[selected_gene_sets, , drop = FALSE]
      }

      # Move the row names to an actual column

      gst <- data.frame(gst, check.names = FALSE)
      gst$gene_set_id <- rownames(gst)
      gst <- gst[, c("gene_set_id", colnames(gst)[colnames(gst) != "gene_set_id"]), drop = FALSE]

      # Apply the user's filters

      gst <- gst[gst$gene_set_id %in% passing_gene_set_ids(gst, col_map, input$pval, input$fdr), , drop = FALSE]

      validate(need(nrow(gst) > 0, "No results matching specified filters"))

      if (nrow(gst) > 0) {
        # Add in the differential genes

        ct <- contrast_reactives$filteredContrastsTables()[[1]][[1]]
        up <- convert_ids(rownames(ct)[ct[["Fold change"]] >= 0], ese, ese@labelfield)
        down <- convert_ids(rownames(ct)[ct[["Fold change"]] < 0], ese, ese@labelfield)

        gene_sets <- genesetselect_reactives$getGeneSets()

        gst$significant_genes <- apply(gst, 1, function(row) {
          direction_genes <- if (row[col_map$direction] == "Up") up else down
          siggenes <- intersect(gene_sets[[gene_set_types]][[row["gene_set_id"]]], direction_genes)
          paste(siggenes, collapse = " ")
        })

        gst
      }
    })

    # Take the table and add links etc

    getDisplayGeneSetAnalysis <- reactive({
      gst <- getGeneSetAnalysis()

      # Add links, but use a prettified version of the gene set name that re-flows to take up less space

      gst <- linkMatrix(gst, eselist@url_roots, data.frame(gene_set_id = prettify_gene_set_name(gst$gene_set_id)))
      colnames(gst) <- prettify_variable_name(colnames(gst))

      gst
    })

    # Make an explanatory file name

    makeFileName <- reactive({
      gsub("[^a-zA-Z0-9_]", "_", paste("gsa", contrast_reactives$getSelectedContrastNames(), genesetselect_reactives$getGeneSetTypes()))
    })

    # Pass the matrix to the simpletable module for display

    simpletable("genesetanalysistable", downloadMatrix = getGeneSetAnalysis, displayMatrix = getDisplayGeneSetAnalysis, filename = makeFileName, rownames = FALSE)
  })
}

#' Row names of an enrichment table passing the p value/FDR thresholds
#'
#' @param gst An enrichment table (data frame or matrix), row-named by gene
#' set ID.
#' @param col_map A column mapping as returned by \code{resolve_enrichment()},
#' with \code{pvalue} and \code{fdr} entries naming the relevant columns of
#' \code{gst}.
#' @param pval Maximum p value to pass.
#' @param fdr Maximum FDR to pass.
#'
#' @return output The row names of \code{gst} whose p value and FDR are both
#' below the given thresholds.
#' @noRd
passing_gene_set_ids <- function(gst, col_map, pval, fdr) {
  rownames(gst)[gst[[col_map$pvalue]] < pval & gst[[col_map$fdr]] < fdr]
}
