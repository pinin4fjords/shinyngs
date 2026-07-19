topgeneboxplot_modal <- list(id = "topgeneboxplot", title = "Top gene boxplots")

# Ranking options offered by the topgeneboxplot module's "Rank genes by"
# control. Each combines a contrast-table column, an optional transform (e.g.
# abs() for "Absolute fold change", which surfaces the biggest movers in
# either direction together) and a sort direction into one dropdown entry, so
# there's a single control rather than a metric picker plus a separate
# ascending/descending toggle. Ordered so that, whichever of q value/p
# value/fold change is unavailable for a given contrast, the first remaining
# option is still a sensible default (most significant, or failing that most
# extreme, first).
topgene_rank_options <- list(
  list(key = "qvalue_asc", column = "q value", label = "q value (ascending)", metric_label = "q value", transform = identity, decreasing = FALSE),
  list(key = "qvalue_desc", column = "q value", label = "q value (descending)", metric_label = "q value", transform = identity, decreasing = TRUE),
  list(key = "pvalue_asc", column = "p value", label = "p value (ascending)", metric_label = "p value", transform = identity, decreasing = FALSE),
  list(key = "pvalue_desc", column = "p value", label = "p value (descending)", metric_label = "p value", transform = identity, decreasing = TRUE),
  list(key = "fc_abs_desc", column = "Fold change", label = "Absolute fold change (descending)", metric_label = "|fold change|", transform = abs, decreasing = TRUE),
  list(key = "fc_abs_asc", column = "Fold change", label = "Absolute fold change (ascending)", metric_label = "|fold change|", transform = abs, decreasing = FALSE),
  list(key = "fc_desc", column = "Fold change", label = "Fold change (descending)", metric_label = "fold change", transform = identity, decreasing = TRUE),
  list(key = "fc_asc", column = "Fold change", label = "Fold change (ascending)", metric_label = "fold change", transform = identity, decreasing = FALSE)
)

#' Ranking options from \code{topgene_rank_options} whose contrast-table
#' column is actually present
#'
#' @param available_columns Character vector of column names present in the
#'   contrast table being ranked
#'
#' @return A subset of \code{topgene_rank_options}
#'
#' @keywords internal
topgeneRankOptions <- function(available_columns) {
  Filter(function(opt) opt$column %in% available_columns, topgene_rank_options)
}

# Pixel budget for laying out plotly_topgene_boxplots()'s subplot grid: a
# fixed content height per row of boxes, plus a fixed gap between rows sized
# to fit each facet's x axis tick labels and (multi-line) title/annotation
# text. These are fixed absolute pixel targets rather than a constant margin
# *fraction* of the total figure, because a fixed fraction shrinks in
# absolute terms as the figure gets shorter (fewer rows) - which is what let
# a low gene count's rows visually overlap. The values below (and the legend
# y offset in plotly_topgene_boxplots()) were tuned empirically against
# rendered output rather than derived analytically, since plotly's own
# spacing between an axis title and a legend anchored below it isn't exposed
# as a documented, computable quantity; they may need revisiting if the
# facet title's font size or line count changes materially.
topgeneboxplot_row_px <- 220
topgeneboxplot_gap_px <- 230
topgeneboxplot_outer_top_px <- 30
topgeneboxplot_outer_bottom_px <- 170

#' Compute the total plot height and inter-row margin fraction needed to lay
#' out \code{n_genes} faceted boxplots over \code{ncol} columns without rows
#' overlapping
#'
#' @param n_genes Number of gene facets being drawn
#' @param ncol Number of facet columns
#'
#' @return A list with \code{nrows}, \code{height} (total plot height in
#'   pixels) and \code{vertical_margin} (top/bottom margin fraction to pass
#'   to \code{\link[plotly]{subplot}} so the gap between rows is
#'   \code{topgeneboxplot_gap_px} regardless of \code{height})
#'
#' @keywords internal
topgeneBoxplotLayout <- function(n_genes, ncol) {
  nrows <- ceiling(n_genes / max(ncol, 1))
  height <- topgeneboxplot_outer_top_px + topgeneboxplot_outer_bottom_px + nrows * topgeneboxplot_row_px + max(0, nrows - 1) * topgeneboxplot_gap_px
  vertical_margin <- if (nrows > 1) topgeneboxplot_gap_px / (2 * height) else 0.02

  list(nrows = nrows, height = height, vertical_margin = vertical_margin)
}

#' Log2-transform an assay matrix and subset/order it to the requested genes
#'
#' Zeros are mapped to log2(1) = 0 rather than dropped to NA (the default
#' \code{rmzeros = FALSE}): unlike a per-sample boxplot, where dropping a
#' handful of zero values out of thousands of genes is harmless, a gene can
#' legitimately be all-zero in one whole condition - exactly the genes an
#' absolute-fold-change ranking surfaces - and dropping those to NA would
#' erase that side's box entirely instead of showing it pinned at zero.
#'
#' @inheritParams ggplot_topgene_boxplots
#'
#' @return The transformed matrix, subset to \code{genes} in that order
#'
#' @keywords internal
topgeneTransformAssay <- function(assay, genes, should_transform = NULL) {
  assay <- cond_log2_transform_matrix(as.matrix(assay), should_transform = should_transform)
  assay[genes, , drop = FALSE]
}

#' The input function of the topgeneboxplot module
#'
#' This module shows a faceted boxplot, one panel per gene, for the top
#' differential genes in a selected contrast (grouped by that contrast's
#' condition, with an optional beeswarm point overlay). Gene selection reuses
#' the \code{\link{selectmatrix}} and \code{\link{contrasts}} modules used
#' elsewhere for expression/contrast selection, plus \code{\link{colormaker}}
#' for palette choice.
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
#' topgeneboxplotInput("topgeneboxplot", eselist)
#'
topgeneboxplotInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("expression"), eselist, require_contrast_stats = TRUE)

  plot_filters <- list(
    uiOutput(ns("rank_by_ui")),
    sliderInput(ns("n_genes"), withHelpIcon("Number of top genes to plot:", "Genes passing the significance filters below are ranked by the chosen metric; this many, from most to least extreme, are shown."), min = 2, max = 50, value = 12),
    checkboxInput(ns("beeswarm"), "Show individual points (beeswarm)", value = TRUE),
    colormakerInput(ns("palette"))
  )

  field_sets <- list(contrasts = contrastsInput(ns("contrasts")), plot = plot_filters, expression = expression_filters)

  fieldSets(ns("fieldset"), field_sets)
}

#' The output function of the topgeneboxplot module
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' topgeneboxplotOutput("topgeneboxplot")
#'
topgeneboxplotOutput <- function(id) {
  ns <- NS(id)
  moduleMain(
    "Top gene boxplots",
    uiOutput(ns("plot_ui")),
    help = modalInput(ns(topgeneboxplot_modal$id), "help", "help")
  )
}

#' The server function of the topgeneboxplot module
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
#' topgeneboxplot("topgeneboxplot", eselist)
#'
topgeneboxplot <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(topgeneboxplot_modal$id, topgeneboxplot_modal$title)

    selectmatrix_reactives <- selectmatrix("expression", eselist,
      select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE,
      require_contrast_stats = TRUE
    )
    contrast_reactives <- contrasts("contrasts", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE)
    getPalette <- colormaker("palette", getNumberCategories = reactive(2))

    # Offer only the ranking options whose underlying column is actually
    # present in the contrast table (p values in particular aren't always
    # supplied alongside q values)

    output$rank_by_ui <- renderUI({
      ct <- contrast_reactives$filteredContrastsTables()[[1]][[1]]
      options <- topgeneRankOptions(colnames(ct))
      validate(need(length(options) > 0, "No ranking metric (q value, p value or fold change) is available for this contrast"))

      choices <- stats::setNames(
        vapply(options, function(opt) opt$key, character(1)),
        vapply(options, function(opt) opt$label, character(1))
      )
      selectInput(session$ns("rank_by"), "Rank genes by", choices, selected = choices[1])
    })

    getRankOption <- reactive({
      key <- input$rank_by
      validate(need(!is.null(key), "Waiting for a ranking option"))

      opt <- Find(function(o) identical(o$key, key), topgene_rank_options)
      validate(need(!is.null(opt), "Unknown ranking option"))
      opt
    })

    # Rank the genes passing the contrast's significance filters by the chosen option (capping to the requested number happens in getTopGeneIds() below)

    getRankedContrastsTable <- reactive({
      ct <- contrast_reactives$filteredContrastsTables()[[1]][[1]]
      opt <- getRankOption()

      validate(need(opt$column %in% colnames(ct), paste0(opt$column, " is required to rank genes for this plot")))
      validate(need(nrow(ct) > 0, "No genes meet the significance filters for this contrast"))

      rank_values <- opt$transform(ct[[opt$column]])
      ct[order(rank_values, decreasing = opt$decreasing), , drop = FALSE]
    })

    getTopGeneIds <- reactive({
      ct <- getRankedContrastsTable()
      head(rownames(ct), input$n_genes)
    })

    # Derive the two-group condition split directly from the selected contrast, rather than an independently-chosen grouping variable, so the plot always
    # reflects the same comparison the gene ranking came from

    getContrastGroups <- reactive({
      selected_contrasts <- contrast_reactives$getSelectedContrasts()
      validate(need(length(selected_contrasts) == 1 && length(selected_contrasts[[1]]) == 1, "Select a single contrast to plot"))
      selected_contrasts[[1]][[1]]
    })

    getContrastSampleGroups <- reactive({
      cont <- getContrastGroups()
      contrast_samples <- contrast_reactives$getSelectedContrastSamples()[[1]][[1]]

      stats::setNames(
        c(rep(cont[["Group.1"]], length(contrast_samples[[1]])), rep(cont[["Group.2"]], length(contrast_samples[[2]]))),
        c(contrast_samples[[1]], contrast_samples[[2]])
      )
    })

    # Reactive so idToLabel() (which converts the full gene annotation table
    # to look up a handful of ids) only runs once per change to the ranked
    # gene set, not once per reader

    getTopGeneLabels <- reactive({
      idToLabel(getTopGeneIds(), selectmatrix_reactives$getExperiment(), sep = "<br />")
    })

    getAnnotations <- reactive({
      ct <- getRankedContrastsTable()
      rows <- getTopGeneIds()
      opt <- getRankOption()

      values <- paste0(opt$metric_label, " = ", signif(opt$transform(ct[rows, opt$column]), 3))

      # Always surface q value too, when it isn't already the ranking column, since it's the standard significance reference
      if (opt$column != "q value" && "q value" %in% colnames(ct)) {
        values <- paste0(values, ", q value = ", signif(ct[rows, "q value"], 3))
      }

      stats::setNames(values, rows)
    })

    # Facet columns and plot height are derived via the same
    # topgeneBoxplotLayout() helper plotly_topgene_boxplots() itself uses, so
    # the reserved plotlyOutput height can never drift out of sync with the
    # subplot grid actually being drawn

    getNcol <- reactive({
      min(3, length(getTopGeneIds()))
    })

    plotHeight <- reactive({
      topgeneBoxplotLayout(length(getTopGeneIds()), getNcol())$height
    })

    output$plot_ui <- renderUI({
      shinycssloaders::withSpinner(plotlyOutput(session$ns("topgeneBoxplot"), height = plotHeight()), color = shinyngsSpinnerColor())
    })

    output$topgeneBoxplot <- renderPlotly({
      withProgress(message = "Making top gene boxplots", value = 0, {
        rows <- getTopGeneIds()
        sample_groups <- getContrastSampleGroups()

        # Read the raw assay matrix directly and subset to the handful of
        # ranked genes before anything else touches it, rather than going via
        # selectMatrix() (which would round the full gene x sample matrix
        # first) - the log2 transform in plotly_topgene_boxplots() doesn't
        # need pre-rounded input anyway
        expression <- selectmatrix_reactives$getAssayMatrix()[rows, names(sample_groups), drop = FALSE]

        # `rows` (real gene ids) drives the facet/matrix lookup; the pretty
        # display string goes through the separate `labels` argument
        plotly_topgene_boxplots(expression, sample_groups, rows,
          annotations = getAnnotations(), labels = stats::setNames(getTopGeneLabels(), rows),
          beeswarm = input$beeswarm, ncol = getNcol(),
          palette = getPalette(), expressiontype = selectmatrix_reactives$getAssayMeasure()
        ) %>%
          shinyngsPlotlyConfig("topgene_boxplots", format = session$userData$plotFormat())
      })
    })
  })
}

#' Make a faceted boxplot of the top differential genes in a contrast
#'
#' Draws one boxplot facet per gene, samples grouped by condition, with an
#' optional beeswarm overlay of individual points. This function only
#' renders: gene ranking, significance filtering and q values are computed
#' upstream by the caller (e.g. the \code{\link{contrasts}} module).
#'
#' @param assay Numeric matrix, genes (rows) by samples (columns)
#' @param groupby Vector of group labels, one per column of \code{assay}
#' @param genes Character vector of row names of \code{assay} to facet on, in
#'   the order facets should appear. Also used to look up values in
#'   \code{assay}, so must match its row names even when \code{labels} is
#'   supplied.
#' @param annotations Optional named character vector keyed by the values in
#'   \code{genes}, rendered as a per-facet annotation (e.g. a q value string)
#' @param labels Optional named character vector keyed by the values in
#'   \code{genes}, used as the facet title in place of the raw gene identifier
#'   (e.g. a gene symbol where \code{genes} holds Ensembl IDs). Genes missing
#'   from \code{labels} fall back to their raw identifier. \code{genes} itself
#'   still drives the lookup into \code{assay} and the matching of
#'   \code{annotations}.
#' @param beeswarm Overlay individual points using
#'   \code{\link[ggbeeswarm]{geom_quasirandom}}?
#' @param ncol Number of facet columns. Defaults to
#'   \code{min(3, length(genes))}
#' @param palette Palette of colours, one for each unique value of
#'   \code{groupby}
#' @param palette_name Valid R color palette name
#' @param expressiontype Expression type for use in y axis label
#' @param base_size Passed to ggplot's \code{theme()}
#' @param should_transform A boolean indicating if the log2 transformation
#'   should be applied. If TRUE, log2 transformation is applied
#'   unconditionally. If FALSE, no transformation is applied. If NULL, a
#'   conditional transformation based on threshold is applied.
#'
#' @return output A \code{ggplot} output
#'
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @export
#'
#' @examples
#' require(airway)
#' data(airway, package = "airway")
#' mat <- assays(airway)[[1]][1:4, ]
#' groupby <- as.character(colData(airway)$dex)
#' ggplot_topgene_boxplots(mat, groupby, rownames(mat))
#'
ggplot_topgene_boxplots <- function(assay, groupby, genes, annotations = NULL, labels = NULL, beeswarm = TRUE, ncol = NULL, palette = NULL, palette_name = COLORBLIND_PALETTE_NAME,
                                     expressiontype = "expression", base_size = 11, should_transform = NULL) {
  plotdata <- topgeneBoxplotData(assay, groupby, genes, should_transform = should_transform)

  if (is.null(ncol)) {
    ncol <- min(3, length(genes))
  }

  palette <- resolvePalette(palette, levels(plotdata$group), palette_name)

  p <- ggplot(plotdata, aes(group, value, fill = group)) +
    geom_boxplot(outlier.shape = if (beeswarm) NA else 19, alpha = if (beeswarm) 0.6 else 1) +
    scale_fill_manual(name = "Group", values = palette)

  if (beeswarm) {
    p <- p + ggbeeswarm::geom_quasirandom(aes(color = group), width = 0.2, size = 1.5, alpha = 0.8) +
      scale_color_manual(values = palette, guide = "none")
  }

  p <- p + facet_wrap(~gene, ncol = ncol, scales = "free_y", labeller = as_labeller(topgeneFacetLabels(labels, genes)))

  ann_df <- topgeneAnnotationData(annotations, genes)
  if (!is.null(ann_df)) {
    p <- p + geom_text(data = ann_df, aes(x = -Inf, y = Inf, label = label), hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE)
  }

  p + theme_bw(base_size = base_size) + theme(
    axis.title.x = element_blank(), legend.position = "bottom", strip.text = element_text(size = 10)
  ) + ylab(splitStringToFixedwidthLines(paste0(
    "log2(",
    prettifyVariablename(expressiontype), ")"
  ), 15))
}

#' Make an interactive faceted boxplot of the top differential genes in a
#' contrast
#'
#' The \code{plotly} equivalent of \code{\link{ggplot_topgene_boxplots}}. One
#' subplot is drawn per gene, with samples grouped by condition. The beeswarm
#' overlay is approximated using \code{plotly}'s native jittered box points,
#' since all of the (typically few) per-sample values are drawn and shipping
#' \code{ggbeeswarm}'s point layout to the browser isn't necessary here.
#'
#' @inheritParams ggplot_topgene_boxplots
#'
#' @return output A \code{plotly} output
#'
#' @export
#'
#' @examples
#' require(airway)
#' data(airway, package = "airway")
#' mat <- assays(airway)[[1]][1:4, ]
#' groupby <- as.character(colData(airway)$dex)
#' plotly_topgene_boxplots(mat, groupby, rownames(mat))
#'
plotly_topgene_boxplots <- function(assay, groupby, genes, annotations = NULL, labels = NULL, beeswarm = TRUE, ncol = NULL, palette = NULL, palette_name = COLORBLIND_PALETTE_NAME,
                                     expressiontype = "expression", should_transform = NULL) {
  assay <- topgeneTransformAssay(assay, genes, should_transform = should_transform)

  group_levels <- unique(na.replace(as.character(groupby), "N/A"))
  groups <- na.replace(as.character(groupby), "N/A")
  palette <- resolvePalette(palette, group_levels, palette_name)

  if (is.null(ncol)) {
    ncol <- min(3, length(genes))
  }
  layout_spec <- topgeneBoxplotLayout(length(genes), ncol)
  nrows <- layout_spec$nrows

  annotations <- topgeneAnnotationVector(annotations, genes)
  facet_titles <- topgeneFacetLabels(labels, genes)
  yaxis_title <- splitStringToFixedwidthLines(paste0("log2(", prettifyVariablename(expressiontype), ")"), 15)

  facet_plots <- lapply(seq_along(genes), function(i) {
    gene <- genes[i]
    values <- assay[gene, ]

    # Setting height via plot_ly() here (rather than a trailing layout() call
    # on the merged subplot) avoids plotly's "specifying width/height in
    # layout() is deprecated" warning; subplot() propagates it from any of
    # the merged plots to the final figure
    p <- plot_ly(height = layout_spec$height)
    for (g in group_levels) {
      p <- add_trace(
        p,
        type = "box", y = values[groups == g], name = g, legendgroup = g,
        boxpoints = if (beeswarm) "all" else "outliers", jitter = 0.4, pointpos = 0,
        fillcolor = palette[[g]], line = list(color = "black"),
        marker = list(color = palette[[g]]),
        showlegend = i == 1
      )
    }

    title <- facet_titles[[gene]]
    if (!is.na(annotations[i])) {
      title <- paste0(title, "<br>", annotations[i])
    }

    # Every row's leftmost facet gets the y axis title, not just the very
    # first facet overall, so the units are legible without scrolling back
    # up to row one
    is_row_start <- (i - 1) %% ncol == 0

    p %>%
      layout(
        xaxis = list(title = title, automargin = TRUE),
        yaxis = list(title = if (is_row_start) yaxis_title else NULL, zeroline = FALSE)
      )
  })

  if (length(facet_plots) == 1) {
    p <- facet_plots[[1]]
  } else {
    p <- subplot(facet_plots,
      nrows = nrows, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE,
      margin = c(0.03, 0.03, layout_spec$vertical_margin, layout_spec$vertical_margin)
    )
  }

  # The outer top/bottom margins are absolute pixels (unlike subplot()'s
  # fraction-based inter-row margin above), and need to actually reserve the
  # topgeneboxplot_outer_top_px/topgeneboxplot_outer_bottom_px budget
  # topgeneBoxplotLayout() assumed when computing the total figure height -
  # otherwise the legend (anchored below the plotting area) can end up
  # overlapping the bottom row's axis title regardless of how tall the
  # overall figure is made
  p %>%
    layout(
      legend = list(title = list(text = "Group"), orientation = "h", xanchor = "center", x = 0.5, y = -0.6),
      margin = list(t = topgeneboxplot_outer_top_px, b = topgeneboxplot_outer_bottom_px), hovermode = "closest"
    )
}

#' Reshape an assay matrix into long form for \code{\link{ggplot_topgene_boxplots}}
#'
#' @inheritParams ggplot_topgene_boxplots
#'
#' @return A data frame with columns \code{gene} (factor, levels = \code{genes}),
#' \code{group} (factor, first-seen order) and \code{value}
#'
#' @keywords internal
topgeneBoxplotData <- function(assay, groupby, genes, should_transform = NULL) {
  assay <- topgeneTransformAssay(assay, genes, should_transform = should_transform)

  group_levels <- unique(na.replace(as.character(groupby), "N/A"))
  groups <- factor(na.replace(as.character(groupby), "N/A"), levels = group_levels)

  plotdata <- do.call(rbind, lapply(genes, function(gene) {
    data.frame(gene = gene, group = groups, value = as.numeric(assay[gene, ]), stringsAsFactors = FALSE)
  }))

  plotdata$gene <- factor(plotdata$gene, levels = genes)
  plotdata
}

#' Build the per-facet annotation data frame used by
#' \code{\link{ggplot_topgene_boxplots}}
#'
#' @inheritParams ggplot_topgene_boxplots
#'
#' @return A data frame with columns \code{gene} (factor, levels = \code{genes})
#' and \code{label}, or \code{NULL} if no annotations were supplied/matched
#'
#' @keywords internal
topgeneAnnotationData <- function(annotations, genes) {
  if (is.null(annotations)) {
    return(NULL)
  }

  matched <- intersect(names(annotations), genes)
  if (length(matched) == 0) {
    return(NULL)
  }

  data.frame(gene = factor(matched, levels = genes), label = unname(annotations[matched]), stringsAsFactors = FALSE)
}

#' Build a per-gene annotation vector, in \code{genes} order, for
#' \code{\link{plotly_topgene_boxplots}}
#'
#' @inheritParams ggplot_topgene_boxplots
#'
#' @return A character vector parallel to \code{genes} (\code{NA} where no
#' annotation was supplied/matched)
#'
#' @keywords internal
topgeneAnnotationVector <- function(annotations, genes) {
  if (is.null(annotations)) {
    return(rep(NA_character_, length(genes)))
  }

  unname(annotations[genes])
}

#' Build the named vector of facet titles used by
#' \code{\link{ggplot_topgene_boxplots}} and \code{\link{plotly_topgene_boxplots}}
#'
#' Genes with no entry in \code{labels} (or when \code{labels} is \code{NULL})
#' fall back to their raw identifier, so callers always get one title per
#' gene regardless of annotation coverage.
#'
#' @inheritParams ggplot_topgene_boxplots
#'
#' @return A character vector parallel to \code{genes}, named by \code{genes}
#'
#' @keywords internal
topgeneFacetLabels <- function(labels, genes) {
  resolved <- if (is.null(labels)) rep(NA_character_, length(genes)) else unname(labels[genes])
  resolved[is.na(resolved)] <- genes[is.na(resolved)]
  stats::setNames(resolved, genes)
}
