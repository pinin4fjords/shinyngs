dexseqtable_modal <- list(id = "dexseqtable", title = "Differential exon usage table")

#' The UI input function of the dexseqtable module
#'
#' This module produces a differential exon usage table based on the output
#' \code{DEXSeqResults} object of the DEXSeq package.
#'
#' For the table to be displayed, the \code{dexseq_results} slot must be filled
#' on at least one of the component \code{ExploratorySummarizedExperiment} objects
#' of the input \code{ExploratorySummarizedExperimentList}.
#'
#' \code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
#' to the contrasts listed in the \code{contrasts} slot of the
#' \code{ExploratorySummarizedExperiment}.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param allow_filtering Allow user filtering of results (default: TRUE)?
#'   Deactivated by the \code{dexseqplot} module, which uses it for showing
#'   gene-specific results.
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' dexseqtableInput("experiment", eselist)
#'
dexseqtableInput <- function(id, eselist, allow_filtering = TRUE) {
  ns <- NS(id)
  fieldSets(ns("fieldset"), dexseqtableInputFields(id, eselist, allow_filtering = allow_filtering))
}

#' Make input fields for producing a table of differential exon usage.
#' Separated here for re-use by the dexseqplot module
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param allow_filtering Allow user filtering of results (default: TRUE)?
#'   Deactivated by the \code{dexseqplot} module, which uses it for showing
#'   gene-specific results.
#'
#' @return output Named list of \code{shiny.tag} objects

dexseqtableInputFields <- function(id, eselist, allow_filtering = TRUE) {
  ns <- NS(id)

  # Only consider experiments with DEXSeq results

  eselist <- eselist[unlist(lapply(eselist, function(ese) has_slot_data(ese, "dexseq_results")))]

  field_sets <- list(differential_exon_usage = list(selectmatrixInput(ns("expression"), eselist), contrastsInput(ns("deuContrast"),
    allow_filtering = allow_filtering,
    summarise = FALSE
  )), export = simpletableInput(ns("dexseqtable"), tabletitle = "DEU"))

  if (allow_filtering) {
    field_sets$differential_exon_usage <- c(field_sets$differential_exon_usage, list(checkboxInput(ns("deuMostSigExon"), "Show most significant exon only per gene?",
      value = TRUE
    )))
  }

  field_sets
}

#' The output function of the dexseqtable module
#'
#' This module produces a differential exon usage table based on the output
#' \code{DEXSeqResults} object of the DEXSeq package.
#'
#' For the table to be displayed, the \code{dexseq_results} slot must be filled
#' on at least one of the component \code{ExploratorySummarizedExperiment} objects
#' of the input \code{ExploratorySummarizedExperimentList}.
#'
#' \code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
#' to the contrasts listed in the \code{contrasts} slot of the
#' \code{ExploratorySummarizedExperiment}.
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
#' dexseqtableOutput("experiment")
#'
dexseqtableOutput <- function(id) {
  ns <- NS(id)

  moduleMain(
    "Differential exon usage",
    simpletableOutput(ns("dexseqtable"), spinner = TRUE),
    help = modalInput(ns(dexseqtable_modal$id), "help", "help")
  )
}

#' The server function of the dexseqtable module
#'
#' This module produces a differential exon usage table based on the output
#' \code{DEXSeqResults} object of the DEXSeq package.
#'
#' For the table to be displayed, the \code{dexseq_results} slot must be filled
#' on at least one of the component \code{ExploratorySummarizedExperiment} objects
#' of the input \code{ExploratorySummarizedExperimentList}.
#'
#' \code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
#' to the contrasts listed in the \code{contrasts} slot of the
#' \code{ExploratorySummarizedExperiment}.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param allow_filtering Allow user filtering of results (default: TRUE)?
#'   Deactivated by the \code{dexseqplot} module, which uses it for showing
#'   gene-specific results.
#' @param getDEUGeneID Reactive expression returning a gene ID.
#' @param show_controls Passed to \code{\link{simpletable}}, spcifies whether
#'   the various \code{datatables} controls are displayed (default: TRUE).
#' @param page_length Passed to \code{\link{simpletable}}, spcifies the number
#'   of rows to display (default: 15).
#' @param link_to_deu_plot Link label fields to the plots produced by
#'   \code{\link{dexseqplot}}? (default: TRUE)
#'
#' @keywords shiny
#'
#' @examples
#' dexseqtable("dexseqtable", eselist)
#'
dexseqtable <- function(id, eselist, allow_filtering = TRUE, getDEUGeneID = NULL, show_controls = TRUE, page_length = 15, link_to_deu_plot = TRUE) {
  moduleServer(id, function(input, output, session) {
    modalServer(dexseqtable_modal$id, dexseqtable_modal$title)

    # Only use experiments with gene set analyses available

    eselist <- eselist[unlist(lapply(eselist, function(ese) has_slot_data(ese, "dexseq_results")))]

    # Call the selectmatrix module and unpack the reactives it sends back

    selectmatrix_reactives <- selectmatrix("expression", eselist, select_assays = FALSE, select_samples = FALSE, select_genes = FALSE)

    # Just use the contrasts module to select a comparison

    contrast_reactives <- contrasts("deuContrast", eselist = eselist, multiple = FALSE, selectmatrix_reactives = selectmatrix_reactives)

    makeDEUTables <- reactive({
      validate(need(requireNamespace("DEXSeq", quietly = TRUE), "The DEXSeq package must be installed to view differential exon usage tables."))

      ese <- selectmatrix_reactives$getExperiment()

      withProgress(message = "Making DEU table for each contrast", value = 0, {
        deu_tables <- lapply(seq_along(ese@dexseq_results), function(contrast) {
          d <- ese@dexseq_results[[contrast]]

          # Add the mean values for the counts

          counts <- DEXSeq::counts(d, normalized = TRUE)
          contrast_samples <- contrast_reactives$getContrastSamples()
          selected_contrast_samples <- contrast_samples[[contrast]]

          mean_counts <- lapply(selected_contrast_samples, function(scs) {
            rowMeans(counts[, scs, drop = FALSE])
          })

          d$mean1 <- round(mean_counts[[1]], 2)
          d$mean2 <- round(mean_counts[[2]], 2)

          # Make fold changes more useful

          fccol <- grep("log2fold", colnames(d), value = TRUE)
          d[, fccol] <- 2^d[, fccol]
          d[d[, fccol] < 1 & !is.na(d[, fccol]), fccol] <- -1 / d[d[, fccol] < 1 & !is.na(d[, fccol]), fccol]

          eu_cols <- colnames(d)[c(8, 9)]

          deucols <- c("groupID", "featureID", "mean1", "mean2", eu_cols, fccol, "pvalue", "padj")
          deu_table <- as.data.frame(d[, deucols])
          deu_table[, c(fccol, eu_cols)] <- round(deu_table[, c(fccol, eu_cols)], 2)
          deu_table[, c("pvalue", "padj")] <- signif(deu_table[, c("pvalue", "padj")], 3)

          # Re-order by p value

          deu_table <- deu_table[order(deu_table$padj), ]

          # Make prettier column labels

          colnames(deu_table) <- c(
            "groupID", "Exon", paste0("Mean normalised count (", eu_cols, ")"), paste0("Exon usage (", eu_cols, ")"), "Relative exon usage fold change",
            "P value", "FDR corrected p value"
          )

          # Add in gene symbols

          deu_table$groupID <- gsub("\\+", " ", deu_table$groupID)

          deu_table
        })
      })
    })

    # Select a DEU table for the contrast of interest and filter based on the controls

    makeDEUTable <- reactive({
      deu_tables <- makeDEUTables()
      selected_contrast_number <- contrast_reactives$getSelectedContrastNumbers()[[1]][[1]]
      deu_table <- deu_tables[[as.numeric(selected_contrast_number)]]

      if (allow_filtering) {
        if (input$deuMostSigExon) {
          deu_table <- deu_table[match(unique(deu_table[, 1]), deu_table[, 1]), ]
        }

        fclim <- contrast_reactives$getFoldChange()
        fclim_card <- contrast_reactives$getFoldChangeCard()
        pvallim <- contrast_reactives$getPval()
        pvallim_card <- contrast_reactives$getPvalCard()
        qvallim <- contrast_reactives$getQval()
        qvallim_card <- contrast_reactives$getQvalCard()

        # deu_table <- deu_table[which(deu_table[['FDR corrected p value']] < qvallim & abs(deu_table[['Relative exon usage fold change']]) > fclim), ]

        deu_table <- deu_table[evaluateCardinalFilter(deu_table[["FDR corrected p value"]], qvallim_card, qvallim) & evaluateCardinalFilter(
          deu_table[["P value"]],
          pvallim_card, pvallim
        ) & evaluateCardinalFilter(deu_table[["Relative exon usage fold change"]], fclim_card, fclim), ]
      }

      # If provided, filter by gene symbol

      if (!is.null(getDEUGeneID)) {
        gene_id <- getDEUGeneID()
        deu_table <- deu_table[deu_table$groupID == gene_id, , drop = FALSE]
        deu_table <- deu_table[order(deu_table$Exon), ]
      }

      # Add labels

      ese <- selectmatrix_reactives$getExperiment()
      labelMatrix(deu_table, ese, "groupID", metafields = selectmatrix_reactives$getMetafields())
    })

    # Make a linked version of the table for display. Override the label links so they point to DEU plots rather than gene pages

    makeDisplayDEUTable <- reactive({
      deu_table <- makeDEUTable()
      url_roots <- eselist@url_roots
      if (link_to_deu_plot) {
        ese <- selectmatrix_reactives$getExperiment()
        url_roots[[ese@labelfield]] <- "?deu_gene="
      }
      linkMatrix(deu_table, url_roots)
    })

    # Pass the matrix to the simpletable module for display

    simpletable("dexseqtable", displayMatrix = makeDisplayDEUTable, downloadMatrix = makeDEUTable, filename = "deutable", rownames = FALSE, show_controls = show_controls, pageLength = page_length)

    # Return reactives for the matrix and controls so the same filters can be used in the 'dexseqplot' module

    c(contrast_reactives, list(
      getExperiment = selectmatrix_reactives$getExperiment, getSelectedContrastNumbers = contrast_reactives$getSelectedContrastNumbers,
      getSelectedContrasts = contrast_reactives$getSelectedContrasts
    ))
  })
}
