#' The UI input function of the genesetbarcodeplot module
#'
#' This module leverages gene sets stored in the \code{gene_sets} slot of an
#' \code{ExploratorySummarizedExperimentList} object to produce interactive
#' barcode plots, reproducing the statistics behind Limma's
#' \code{\link[limma]{barcodeplot}} function. Genes are ranked by fold changes
#' calculated with the \code{contrasts} module, and FDR values from the
#' \code{gene_set_analyses} slot of the selected
#' \code{ExploratorySummarizedExperiment} are displayed where provided.
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
#' data(zhangneurons)
#' genesetbarcodeplotInput("myid", zhangneurons)
#'
#' # Almost certainly used via application creation
#'
#' app <- prepareApp("genesetbarcodeplot", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
genesetbarcodeplotInput <- function(id, eselist) {
  ns <- NS(id)

  # Only use experiments with gene set analyses available

  eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]

  # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay select doesn't have invalid options.

  for (exp in names(eselist)) {
    assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
  }

  expression_filters <- selectmatrixInput(ns("expression"), eselist)

  field_sets <- list(gene_set = genesetselectInput(ns("genesetbarcodeplot"), multiple = FALSE), contrast = contrastsInput(ns("genesetbarcodeplot"), allow_filtering = FALSE))

  # Things we don't want to wrap in a field set - probably hidden stuff

  naked_fields <- list()

  if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
    field_sets$select_assay_data <- expression_filters
  } else {
    naked_fields <- pushToList(naked_fields, expression_filters)
  }

  field_sets <- c(field_sets, list(export = list(p(simpletableInput(ns("genesetbarcodeplot"), "Gene set")))))

  list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The output function of the genesetbarcodeplot module
#'
#' This module leverages gene sets stored in the \code{gene_sets} slot of an
#' \code{ExploratorySummarizedExperimentList} object to produce interactive
#' barcode plots, reproducing the statistics behind Limma's
#' \code{\link[limma]{barcodeplot}} function. Genes are ranked by fold changes
#' calculated with the \code{contrasts} module, and FDR values from the
#' \code{gene_set_analyses} slot of the selected
#' \code{ExploratorySummarizedExperiment} are displayed where provided.
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' genesetbarcodeplotOutput("experiment")
#'
#' # Almost certainly used via application creation
#'
#' app <- prepareApp("genesetbarcodeplot", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
genesetbarcodeplotOutput <- function(id) {
  ns <- NS(id)

  list(
    modalInput(ns("genesetbarcodeplot"), "help", "help"), modalOutput(ns("genesetbarcodeplot"), "Gene set barcode plot", includeMarkdown(system.file("inlinehelp",
      "genesetbarcodeplot.md",
      package = packageName()
    ))), h3("Gene set barcode plot"), plotlyOutput(ns("genesetbarcodeplot"), height = "460px"), h4("Gene set differential expression"),
    simpletableOutput(ns("genesetbarcodeplot"))
  )
}

#' The server function of the genesetbarcodeplot module
#'
#' This module leverages gene sets stored in the \code{gene_sets} slot of an
#' \code{ExploratorySummarizedExperimentList} object to produce interactive
#' barcode plots, reproducing the statistics behind Limma's
#' \code{\link[limma]{barcodeplot}} function. Genes are ranked by fold changes
#' calculated with the \code{contrasts} module, and FDR values from the
#' \code{gene_set_analyses} slot of the selected
#' \code{ExploratorySummarizedExperiment} are displayed where provided.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' genesetbarcodeplot("genesetbarcodeplot", eselist)
#'
#' # Almost certainly used via application creation
#'
#' app <- prepareApp("genesetbarcodeplot", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
genesetbarcodeplot <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    # Only use experiments with gene set analyses available

    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]

    # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay select doesn't have invalid options.

    for (exp in names(eselist)) {
      assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
    }

    # Call the selectmatrix module and unpack the reactives it sends back

    selectmatrix_reactives <- selectmatrix("expression", eselist, select_samples = FALSE, select_genes = FALSE, select_meta = FALSE)
    unpack.list(selectmatrix_reactives)

    # Pass the matrix to the contrasts module for processing

    unpack.list(contrasts("genesetbarcodeplot", eselist = eselist, multiple = FALSE, selectmatrix_reactives = selectmatrix_reactives))

    # Parse the gene sets for ease of use

    unpack.list(genesetselect("genesetbarcodeplot", eselist, getExperiment, multiple = FALSE))

    observe({
      updateGeneSetsList()
    })

    # Make a sensible title for the plot

    barcodeplotTitle <- reactive({
      ese <- getExperiment()

      title_components <- c(prettifyGeneSetName(unlist(getGenesetNames())), getSelectedContrastNames()[[1]])

      gene_set_types <- getGenesetTypes()
      assay <- getAssay()
      gene_set_names <- getGenesetNames()
      contrast_numbers <- as.numeric(getSelectedContrastNumbers()[[1]][[1]])

      enrichment <- resolve_enrichment(ese, assay, gene_set_types, contrast_numbers, eselist@contrasts[[contrast_numbers]])

      if (!is.null(enrichment) && all(gene_set_names %in% rownames(enrichment$gst))) {
        gst <- enrichment$gst
        col_map <- enrichment$col_map
        fdr <- paste(signif(gst[gene_set_names, col_map$fdr], 3), collapse = ",")
        direction <- paste(gst[gene_set_names, col_map$direction], collapse = ",")
        title_components <- c(title_components, paste(paste("Direction:", direction), paste("FDR:", fdr)))
      } else {
        title_components <- c(title_components, "(no association)")
      }

      plot_title <- paste(title_components, collapse = "\n")

      plot_title
    })

    # Get the list of fold changes by which to rank genes

    getFoldChanges <- reactive({
      ct <- filteredContrastsTables()[[1]][[1]]
      ct$`Fold change`
    })

    # Get gene IDs of the same type as used for gene sets

    getGeneIDs <- reactive({
      convertIds(rownames(filteredContrastsTables()[[1]][[1]]), getExperiment(), eselist@gene_set_id_type)
    })

    # Get display labels for the same genes, for use in plot hover text

    getLabels <- reactive({
      idToLabel(rownames(filteredContrastsTables()[[1]][[1]]), getExperiment())
    })

    # Render the barcode plot

    output$genesetbarcodeplot <- renderPlotly({
      set_genes <- getPathwayGenes()

      plotly_barcodeplot(
        fold_changes = getFoldChanges(), gene_ids = getGeneIDs(), set_gene_ids = names(set_genes), labels = getLabels(),
        plot_title = barcodeplotTitle()
      )
    })

    # Make a table of contrast data for the gene set Subset the linked contrasts table for the gene set genes

    gsbpContrastsTable <- reactive({
      lct <- labelledContrastsTable()
      ese <- getExperiment()
      set_genes <- getPathwayGenes()

      lct[which(lct[[prettifyVariablename(ese@labelfield)]] %in% set_genes), ]
    })

    # Add links for display

    gsbpLinkedContrastsTable <- reactive({
      # Force the table (and its gene-set validation) here rather than letting it
      # evaluate lazily inside linkMatrix()'s colnames() call, where a pending
      # validation would surface as an error instead of a clean prompt.
      contrasts_table <- gsbpContrastsTable()
      linkMatrix(contrasts_table, eselist@url_roots)
    })

    # Provide the gene set genes in a table of contrst data

    simpletable("genesetbarcodeplot", downloadMatrix = gsbpContrastsTable, displayMatrix = gsbpLinkedContrastsTable, filename = "gene_set_contrast", rownames = FALSE, pageLength = 10)

    # Catch the gene set from the URL

    observe({
      query <- parseQueryString(session$clientData$url_search)

      if (length(intersect(c("geneset"), names(query))) == 0) {
        return()
      }

      url_observe <- observe({
        if ("geneset" %in% names(query)) {
          updateGeneset()
        }
        url_observe$suspend()
      })
    })

    updateGeneset
  })
}

#' Tricube-weighted moving average
#'
#' A verbatim port of \code{limma::tricubeMovingAverage()}, used to
#' reproduce the enrichment "worm" curve drawn by
#' \code{\link[limma]{barcodeplot}} without depending on limma's plotting
#' internals. Only uses \code{stats::filter()}.
#'
#' @param x Numeric vector to smooth
#' @param span Proportion of \code{x} to use as the smoothing window
#' @param power Tricube weighting power
#'
#' @return output A smoothed numeric vector the same length as \code{x}
#' @noRd
tricubeMovingAverage <- function(x, span = 0.5, power = 3) {
  if (span > 1) span <- 1
  if (span <= 0) {
    return(x)
  }
  if (power < 0) power <- 0

  n <- length(x)
  width <- span * n
  hwidth <- as.integer(width %/% 2L)
  width <- 2L * hwidth + 1L
  if (width > n) {
    width <- width - 2L
    hwidth <- hwidth - 1L
  }
  if (hwidth <= 0L) {
    return(x)
  }

  u <- seq(from = -1, to = 1, length = width) * width / (width + 1)
  tricube.weights <- (1 - abs(u)^3)^power
  tricube.weights <- tricube.weights / sum(tricube.weights)

  z <- numeric(hwidth)
  x <- as.vector(stats::filter(c(z, x, z), tricube.weights), mode = "numeric")[(hwidth + 1):(n + hwidth)]

  cw <- cumsum(tricube.weights)
  x[1:hwidth] <- x[1:hwidth] / cw[(width - hwidth):(width - 1)]
  x[(n - hwidth + 1):n] <- x[(n - hwidth + 1):n] / cw[(width - 1):(width - hwidth)]
  x
}

#' Make an interactive gene set barcode plot with plotly
#'
#' Reproduces the statistics drawn by \code{\link[limma]{barcodeplot}} (rank
#' ordering, quantile shading and the enrichment "worm" curve) as an
#' interactive \code{plotly} plot: rug ticks mark where gene set members fall
#' in the statistic ranking, and a curve on a secondary axis shows local
#' enrichment relative to the overall rate.
#'
#' @param fold_changes A numeric vector of fold changes (or other ranking
#'   statistic), one per gene
#' @param gene_ids Gene IDs for the values in \code{fold_changes}, in the
#'   same ID space as \code{set_gene_ids}
#' @param set_gene_ids Gene IDs for the gene set
#' @param labels Display labels for the values in \code{fold_changes}, used
#'   in hover text (default: \code{gene_ids})
#' @param plot_title A title for the plot
#' @param worm_span Span passed to the tricube moving average used for the
#'   enrichment curve (default: 0.45, matching limma's default)
#'
#' @return output A plotly plot object
#'
#' @importFrom stats quantile
#' @export

plotly_barcodeplot <- function(fold_changes, gene_ids, set_gene_ids, labels = gene_ids, plot_title = "", worm_span = 0.45) {
  plot_title <- gsub("\n", "<br>", plot_title, fixed = TRUE)

  keep <- !is.na(fold_changes)
  fold_changes <- fold_changes[keep]
  gene_ids <- gene_ids[keep]
  labels <- labels[keep]

  n <- length(fold_changes)
  ord <- order(fold_changes)

  ordered_fc <- fold_changes[ord]
  ordered_labels <- labels[ord]
  idx <- gene_ids[ord] %in% set_gene_ids
  r <- which(idx)

  quantiles <- c(-1, 1) * sqrt(2)
  nneg <- sum(fold_changes < quantiles[1])
  npos <- sum(fold_changes > quantiles[2])

  # Two stacked panels sharing one x-axis: a narrow ranking strip (yaxis2, bottom)
  # carrying the rug ticks and quantile shading, and a taller panel above it
  # (yaxis, top) carrying the enrichment worm curve - mirroring limma's layout
  # rather than overlaying both series in the same space.
  barcode_domain <- c(0, 0.22)

  shapes <- list(
    list(type = "rect", xref = "x", yref = "y2", x0 = 0.5, x1 = nneg + 0.5, y0 = 0, y1 = 1, fillcolor = "lightblue", line = list(width = 0), layer = "below"),
    list(type = "rect", xref = "x", yref = "y2", x0 = nneg + 0.5, x1 = n - npos + 0.5, y0 = 0, y1 = 1, fillcolor = "lightgray", line = list(width = 0), layer = "below"),
    list(type = "rect", xref = "x", yref = "y2", x0 = n - npos + 0.5, x1 = n + 0.5, y0 = 0, y1 = 1, fillcolor = "pink", line = list(width = 0), layer = "below")
  )

  xaxis <- list(
    title = "Statistic", range = c(0.5, n + 0.5), tickmode = "array", anchor = "y2",
    tickvals = round(seq(1, n, length.out = 11)), ticktext = signif(quantile(fold_changes, probs = (0:10) / 10), 3)
  )
  yaxis <- list(title = "Enrichment", domain = c(barcode_domain[2] + 0.15, 1), showgrid = FALSE)
  yaxis2 <- list(title = "", domain = barcode_domain, range = c(0, 1), showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)

  p <- plotly::plot_ly()

  title_lines <- 1 + lengths(regmatches(plot_title, gregexpr("<br>", plot_title)))
  title_margin <- list(t = 45 + 28 * title_lines)

  if (length(r) == 0) {
    p <- plotly::layout(p, title = plot_title, xaxis = xaxis, yaxis = yaxis, yaxis2 = yaxis2, shapes = shapes, margin = title_margin, annotations = list(
      list(text = "No genes from this set found in the ranked list", xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE)
    ))
    return(p)
  }

  tick_text <- paste0(ordered_labels[idx], "<br>Fold change: ", signif(ordered_fc[idx], 3))

  # Sample several y-positions per tick (not just the two endpoints) so hovering
  # anywhere along its vertical extent finds a nearby point - a line trace with
  # only endpoint vertices only registers hover near those endpoints.
  tick_y_template <- c(0, 0.25, 0.5, 0.75, 1, NA)
  p <- plotly::add_lines(p,
    x = as.vector(rbind(r, r, r, r, r, NA_real_)), y = rep(tick_y_template, length(r)),
    text = as.vector(rbind(tick_text, tick_text, tick_text, tick_text, tick_text, NA_character_)), hoverinfo = "text",
    line = list(color = "black", width = 1), name = "Gene set", yaxis = "y2"
  )

  ave_enrich <- length(r) / n
  worm <- tricubeMovingAverage(as.numeric(idx), span = worm_span) / ave_enrich

  p <- plotly::add_lines(p,
    x = seq_len(n), y = worm, hoverinfo = "y", line = list(color = "darkgreen", width = 2), name = "Enrichment", yaxis = "y"
  )

  shapes <- c(shapes, list(list(type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = 1, y1 = 1, line = list(color = "gray", dash = "dash", width = 1))))

  plotly::layout(p,
    title = plot_title, xaxis = xaxis, yaxis = yaxis, yaxis2 = yaxis2, shapes = shapes, margin = title_margin,
    hovermode = "closest", hoverdistance = 100
  )
}
