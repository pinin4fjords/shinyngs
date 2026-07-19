#' The input function of the heatmap module
#'
#' Three types of heatmaps are provided, employed in various places in the
#' rnaseq app (for example), and using much of the same code. Expresssion
#' heatmaps plot expression for samples by column and e.g. genes by row. A
#' samples heatmap plots samples vs samples to illustrate correlation patterns.
#' A pca heatmap plots the results of anova tests applied to examine the
#' associations between principal components and experimental variables.
#'
#' This provides the form elements to control the heatmap display
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param type The type of heatmap that will be made. 'expression', 'samples' or
#'   'pca'
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
#' heatmapInput("heatmap", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("heatmap", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
heatmapInput <- function(id, eselist, type = "expression") {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("heatmap"), eselist)

  # Only provide controls for clustering etc for the expression heat maps

  if (type == "expression") {
    heatmap_filters <- list(h5("Clustering"), checkboxInput(ns("cluster_rows"), "Cluster rows?", TRUE), checkboxInput(
      ns("cluster_cols"), "Cluster columns?",
      FALSE
    ), radioButtons(ns("scale"), "Scale by:", c(Row = "row", Column = "column", None = "none")))
  } else {
    if (type == "pca") {
      cluster_rows <- TRUE
      cluster_cols <- FALSE
    } else {
      cluster_rows <- TRUE
      cluster_cols <- TRUE
    }
    heatmap_filters <- list(hiddenInput(ns("cluster_rows"), cluster_rows), hiddenInput(ns("cluster_cols"), cluster_cols), hiddenInput(ns("scale"), "none"))
  }

  # Output sets of fields in their own containers

  if (type == "pca" && length(eselist@group_vars) == 0) {
    filters <- list(groupbyInput(ns("heatmap"), color = FALSE), heatmap_filters, fieldSets(ns("fieldset"), list(
      expression = expression_filters
    )))
  } else {
    filters <- fieldSets(ns("fieldset"), list(
      heatmap = list(groupbyInput(ns("heatmap"), color = FALSE), heatmap_filters), expression = expression_filters
    ))
  }

  filters
}

# Fixed pixel height for each annotation color-bar row drawn above a heatmap.
# Kept as an absolute pixel value (converted to a fraction of the actual
# container height where needed) rather than a fraction of the plot itself,
# so the bars stay the same size regardless of how many heatmap rows are shown.
HEATMAP_ANNOTATION_ROW_HEIGHT_PX <- 20

heatmap_modal_specs <- list(
  pca = list(id = "pcavsexperiment", title = "Principal components vs experimental variables"),
  samples = list(id = "clusteringheatmap", title = "Sample clustering heatmap"),
  expression = list(id = "expressionheatmap", title = "Expression heatmap")
)

#' The output function of the heatmap module
#'
#' Three types of heatmaps are provided, employed in various places in the
#' rnaseq app (for example), and using much of the same code. Expresssion
#' heatmaps plot expression for samples by column and e.g. genes by row. A
#' samples heatmap plots samples vs samples to illustrate correlation patterns.
#' A pca heatmap plots the results of anova tests applied to examine the
#' associations between principal components and experimental variables.
#'
#' @param id Submodule namespace
#' @param type Heatmap type: 'pca', 'samples' or 'expression'
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' heatmapOutput("heatmap")
#'
#' # Almost certainly used via application creation
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("heatmap", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
heatmapOutput <- function(id, type = "") {
  ns <- NS(id)
  spec <- heatmap_modal_specs[[type]]
  help <- if (is.null(spec)) NULL else modalInput(ns(spec$id), "help", "help")
  moduleMain(NULL, uiOutput(ns("heatmap_ui")), help = help)
}

#' The server function of the heatmap module
#'
#' Three types of heatmaps are provided, employed in various places in the
#' rnaseq app (for example), and using much of the same code. Expresssion
#' heatmaps plot expression for samples by column and e.g. genes by row. A
#' samples heatmap plots samples vs samples to illustrate correlation patterns.
#' A pca heatmap plots the results of anova tests applied to examine the
#' associations between principal components and experimental variables.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param type The type of heatmap that will be made. 'expression', 'samples' or
#'   'pca' (default: 'expression')
#'
#' @importFrom viridisLite viridis
#' @keywords shiny
#'
#' @examples
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   heatmap("heatmap", eselist, type = "pca")
#'   app <- prepareApp("heatmap", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
heatmap <- function(id, eselist, type = "expression") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spec <- heatmap_modal_specs[[type]]
    if (!is.null(spec)) {
      modalServer(spec$id, spec$title)
    }

    # Make the groupby UI element

    groupby_reactives <- groupby("heatmap", eselist = eselist, group_label = "Annotate with variables:", multiple = TRUE)

    # Call the selectmatrix module and hold on to the reactives it sends back

    if (type == "expression") {
      selectmatrix_reactives <- selectmatrix("heatmap", eselist, var_max = 500)
    } else {
      selectmatrix_reactives <- selectmatrix("heatmap", eselist, var_n = 1000, select_meta = FALSE, allow_summarise = FALSE)
    }

    # Render the heatmap container

    output$heatmap_ui <- renderUI({
      withProgress(message = "Preparing heatmap container", value = 0, {
        list(h3(makeTitle()), shinycssloaders::withSpinner(plotly::plotlyOutput(ns("interactiveHeatmap"), height = plotHeight()), color = shinyngsSpinnerColor()))
      })
    })

    # Create a title

    makeTitle <- reactive({
      if (type == "pca") {
        paste("PCA vs variable association plot based on expression matrix:", selectmatrix_reactives$matrixTitle())
      } else if (type == "expression") {
        paste("Expression heat map based on expression matrix:", selectmatrix_reactives$matrixTitle())
      } else {
        paste("Sample clustering heat map based on expression matrix:", selectmatrix_reactives$matrixTitle())
      }
    })

    # Get the experiment data and tidy up as appropriate

    getExperimentData <- reactive({
      if (selectmatrix_reactives$isSummarised()) {
        NULL
      } else {
        ed <- selectmatrix_reactives$selectColData()

        anno_fields <- groupby_reactives$getGroupby()

        if (!is.null(anno_fields)) {
          # Prettify the factor levels for display

          colnames(ed)[match(anno_fields, colnames(ed))] <- prettifyVariablename(anno_fields)
          group_vars <- prettifyVariablename(anno_fields)

          # Make factors from the specified grouping variables
          sm <- selectmatrix_reactives$selectMatrix()
          ed <- ed[colnames(sm), , drop = FALSE]

          ed <- data.frame(lapply(structure(group_vars, names = group_vars), function(x) factor(ed[, x], levels = unique(ed[, x]))),
            check.names = FALSE,
            row.names = rownames(ed)
          )

          # Order by the group variables for display purposes

          ed[do.call(order, as.list(ed[, group_vars, drop = FALSE])), , drop = FALSE]
        } else {
          ed
        }
      }
    })


    # Get a matrix of annotation to use in the plots. This is the experiment data except when type is 'pca', when it's not relevant

    getPlotAnnotation <- reactive({
      if (type == "pca") {
        NULL
      } else {
        getExperimentData()
      }
    })

    # Get a a matrix of the values we actually want the user to see in mouseovers etc.

    getDisplayMatrix <- reactive({
      pm <- selectmatrix_reactives$selectMatrix()

      if (type == "samples") {
        pm <- cor(pm, use = "complete.obs", method = "spearman")
      } else if (type == "pca") {
        pm <- getPCAPvalues()
      }

      # We can't do clustering with anything with the same value in all columns. So take these out.

      if (as.logical(input$cluster_rows) && !is.null(getExperimentData())) {
        pm <- pm[rowsWithMultipleValues(pm), , drop = FALSE]
      }

      # For expression, re-order by the experiment

      if (type == "expression") {
        pm <- pm[, rownames(getExperimentData())]
      }
      pm
    })

    # Create of values to use in plotting, i.e. to define the colors

    getPlotMatrix <- reactive({
      pm <- getDisplayMatrix()

      if (type == "expression") {
        pm <- log2(pm + 1)
      }
      pm
    })

    # Run a PCA with the currently selected matrix, ready for association with
    # experimental variables via anova. Cached on the inputs read below, since
    # prcomp() doesn't need to re-run when e.g. only the clustering/scale
    # controls change.

    getPCAComponents <- reactive({
      pcameta <- getExperimentData()
      pcavals <- selectmatrix_reactives$selectMatrix()[, rownames(pcameta), drop = FALSE]

      pca <- runPCA(pcavals)

      # Check for non-useful variables (those with 1 value, or N values where N is the
      # number of samples)

      informative_variables <- chooseGroupingVariables(pcameta)

      validate(
        need(length(informative_variables) > 0, "Warning: supplied filters have reduced sample metadata selections so as to render all variables uninformative (number of unique values = 1 or N)")
      )

      list(pca_coords = pca$x, pcameta = pcameta, fraction_explained = calculatePCAFractionExplained(pca))
    }) %>% bindCache(getExperimentData(), selectmatrix_reactives$selectMatrix())

    # The raw ANOVA p values, used only to size the plot container ahead of
    # rendering it via plotly_pca_metadata_heatmap(). Cached on getPCAComponents()
    # alone since the anova (unlike prcomp()) is otherwise cheap to rerun, but
    # not so cheap that it should redo the whole ANOVA on every reactive tick.

    getPCAPvalues <- reactive({
      components <- getPCAComponents()
      anova_pca_metadata(pca_coords = components$pca_coords, pcameta = components$pcameta, fraction_explained = components$fraction_explained)
    }) %>% bindCache(getPCAComponents())

    # Calculate heights for the the various types of heatmap

    plotHeight <- reactive({
      display_matrix <- getDisplayMatrix()

      # Allowance for the angled column labels
      xaxis_labels_height <- 150

      (nrow(display_matrix) * rowHeight()) + dendroHeight() + annotationHeight() + xaxis_labels_height
    })

    # Add a chunk for the dendrogram at the top

    dendroHeight <- reactive({
      if (as.logical(input$cluster_cols)) {
        150
      } else {
        0
      }
    })

    # Reserve a fixed number of pixels per annotation variable, so adding more
    # annotation rows doesn't compress the heatmap grid itself

    annotationHeight <- reactive({
      plot_annotation <- getPlotAnnotation()

      if (is.null(plot_annotation) || ncol(plot_annotation) == 0) {
        0
      } else {
        HEATMAP_ANNOTATION_ROW_HEIGHT_PX * ncol(plot_annotation)
      }
    })

    # Small row height for expression heat map (probably lots of rows)

    rowHeight <- reactive({
      if (type == "expression") {
        12
      } else if (type == "pca") {
        20
      } else {
        20
      }
    })

    # Make row labels

    rowLabels <- reactive({
      ese <- selectmatrix_reactives$getExperiment()
      plot_matrix <- getPlotMatrix()

      if (has_slot_data(ese, "labelfield") && type == "expression") {
        annotation <- as.data.frame(mcols(ese))
        labels <- annotation[match(rownames(plot_matrix), annotation[[ese@idfield]]), ese@labelfield]
        labels[!is.na(labels)] <- paste(labels[!is.na(labels)], rownames(plot_matrix)[!is.na(labels)], sep = " / ")
        labels[is.na(labels)] <- rownames(plot_matrix)[is.na(labels)]
        labels
      } else {
        rownames(plot_matrix)
      }
    })

    # Build the heatmap. For the pca type this delegates entirely to
    # plotly_pca_metadata_heatmap(), which owns the p value transform and
    # display settings specific to that plot; the other types keep composing
    # interactiveHeatmap() directly. Cached on exactly the inputs read below,
    # since this covers heatmaply()'s own layout work as well as the
    # row/column clustering it performs internally. plot_height is deliberately
    # not listed as its own cache key: it's fully derived from the other keys
    # already listed (getDisplayMatrix()/getPCAPvalues(), cluster/annotation
    # inputs), and evaluating it just to check the cache would force
    # getDisplayMatrix() (and, for pca, the ANOVA behind it) to rerun regardless
    # of whether anything actually changed.

    getHeatmapPlot <- reactive({
      if (type == "pca") {
        components <- getPCAComponents()
        validateOrCatch(plotly_pca_metadata_heatmap(
          pca_coords = components$pca_coords, pcameta = components$pcameta, fraction_explained = components$fraction_explained,
          cluster_rows = as.logical(input$cluster_rows), plot_height = plotHeight()
        ))
      } else {
        validateOrCatch(interactiveHeatmap(
          plotmatrix = getPlotMatrix(), displaymatrix = getDisplayMatrix(), getPlotAnnotation(), cluster_cols = as.logical(input$cluster_cols),
          cluster_rows = as.logical(input$cluster_rows), scale = input$scale, row_labels = rowLabels(), colors = viridisLite::viridis(100), cexCol = 1,
          cexRow = 1, display_numbers = FALSE, plot_height = plotHeight()
        ))
      }
    })

    if (type == "pca") {
      getHeatmapPlot <- getHeatmapPlot %>% bindCache(getPCAComponents(), input$cluster_rows)
    } else {
      getHeatmapPlot <- getHeatmapPlot %>% bindCache(
        getPlotMatrix(), getDisplayMatrix(), getPlotAnnotation(), input$cluster_cols, input$cluster_rows, input$scale, rowLabels()
      )
    }

    output$interactiveHeatmap <- plotly::renderPlotly({
      withProgress(message = "Building interactive heatmap", value = 0, {
        getHeatmapPlot() %>% shinyngsPlotlyConfig("heatmap", format = session$userData$plotFormat())
      })
    })
  })
}

#' Make an interactive heatmap with heatmaply
#'
#' This is a generic function which may be useful outside of this package. It
#' produces a heatmap based on an expression matrix and accompanying
#' experiment data in the form of a frame, using \code{heatmaply::heatmaply()}.
#'
#' @param plotmatrix Expression/ other data matrix
#' @param displaymatrix A matrix of values shown on hover, in addition to the
#'   (possibly scaled/ transformed) values in \code{plotmatrix}
#' @param sample_annotation A data frame with sample metadata, used to draw
#'   column side colors and an accompanying legend
#' @param cluster_cols Cluster columns?
#' @param cluster_rows Cluster rows?
#' @param scale 'row', 'column' or none
#' @param colors A vector of colors for the heatmap
#' @param row_labels Vector labels to use for rows
#' @param cexCol Character expansion factor passed to \code{heatmaply()}
#' @param cexRow Character expansion factor passed to \code{heatmaply()}
#' @param display_numbers Boolean, should the (possibly scaled/ transformed)
#'   values in \code{plotmatrix} be displayed on the heatmap cells?
#' @param hide_colorbar Boolean, should the color scale legend be hidden?
#' @param plot_height The total rendered height of the plot in pixels, used to
#'   convert the fixed-pixel annotation row height into the fraction
#'   \code{heatmaply()} expects. Should match the \code{height} the plot is
#'   actually rendered at (e.g. the \code{height} argument of the
#'   \code{plotlyOutput()} it's displayed in).
#' @param ... Additional arguments passed to \code{heatmaply()}
#'
#' @return output A plotly htmlwidget as produced by heatmaply()
#'
#'
#' @importFrom viridisLite viridis
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(24), nrow = 6, ncol = 4,
#'   dimnames = list(paste0("gene", 1:6), paste0("s", 1:4)))
#' sample_annotation <- data.frame(
#'   condition = rep(c("treated", "control"), each = 2),
#'   row.names = colnames(mat)
#' )
#' interactiveHeatmap(mat, mat, sample_annotation, row_labels = rownames(mat))
#'
interactiveHeatmap <- function(plotmatrix, displaymatrix, sample_annotation, cluster_rows = TRUE, cluster_cols = FALSE, scale = "row", row_labels, colors = viridisLite::viridis(100), cexCol = 0.7, cexRow = 0.7, display_numbers = FALSE, hide_colorbar = FALSE, plot_height = 600, ...) {
  # should be possible to specify this in the labRow parameter- but the clustering messes it up

  rownames(plotmatrix) <- row_labels

  dendrogram <- "none"
  Rowv <- FALSE
  Colv <- FALSE

  col_side_colors <- NULL
  if ((!is.null(sample_annotation)) && ncol(sample_annotation) > 0) {
    col_side_colors <- sample_annotation[colnames(plotmatrix), , drop = FALSE]
  }

  if (nrow(plotmatrix) < 2) {
    cluster_rows <- FALSE

    # This is a little hack to work around a bug in heatmaply with single-row data
    plotmatrix <- rbind(plotmatrix, plotmatrix)
    displaymatrix <- rbind(displaymatrix, displaymatrix)
  }

  # Specify how the dendrogram should be created

  if (all(cluster_rows, cluster_cols)) {
    dendrogram <- "both"
    Rowv <- calculateDendrogram(t(plotmatrix))
    Colv <- calculateDendrogram(plotmatrix)
  } else if (cluster_rows) {
    dendrogram <- "row"
    Rowv <- calculateDendrogram(t(plotmatrix))
  } else if (cluster_cols) {
    dendrogram <- "column"
    Colv <- calculateDendrogram(plotmatrix)
  }

  # Turn off scaling if there's only 2 possible values in the matrix, otherwise things look a bit odd

  if (length(unique(as.numeric(plotmatrix))) < 3) {
    scale <- "none"
  }

  # heatmaply's plot_method = "plotly" replaces rather than supplements the default
  # row/column/value hover text once custom_hovertext is supplied, so build the full
  # tooltip ourselves to keep the row and column identifiers on hover
  hovertext <- matrix(
    paste0(
      "Row: ", rownames(plotmatrix)[row(plotmatrix)], "<br>",
      "Column: ", colnames(plotmatrix)[col(plotmatrix)], "<br>",
      "Value: ", trimws(formatC(displaymatrix, format = "g", digits = 5))
    ),
    nrow = nrow(plotmatrix), dimnames = dimnames(plotmatrix)
  )

  # heatmaply reserves a fixed 0.2/0.1 fraction of the plot height for the column
  # dendrogram/annotation bars regardless of how many annotation variables there
  # are, which reads as oversized for a small number of them. Give the
  # annotation bars a constant pixel height instead (converted to the fraction
  # heatmaply expects using the plot's actual rendered height), so they don't
  # grow or shrink with the number of heatmap rows, and give the rest of the
  # space to the heatmap itself
  has_col_dend <- dendrogram %in% c("both", "column")
  has_col_annotation <- !is.null(col_side_colors)

  dend_height <- 0.2

  annotation_height <- if (has_col_annotation) (HEATMAP_ANNOTATION_ROW_HEIGHT_PX * ncol(col_side_colors)) / plot_height else 0

  subplot_heights <- NULL
  if (has_col_dend && has_col_annotation) {
    subplot_heights <- c(dend_height, annotation_height, 1 - dend_height - annotation_height)
  } else if (has_col_annotation) {
    subplot_heights <- c(annotation_height, 1 - annotation_height)
  }

  # heatmaply pools the values of every annotation variable into a single
  # discrete color scale, so by default they all share one generically-titled
  # legend. Supply that shared palette ourselves so we can also build a
  # replacement legend split into one titled group per annotation variable.
  col_side_palette <- NULL
  if (has_col_annotation) {
    col_side_palette <- combinedAnnotationColors(col_side_colors)
  }

  p <- heatmaply::heatmaply(plotmatrix,
    dendrogram = dendrogram, custom_hovertext = hovertext, Rowv = Rowv, Colv = Colv, scale = scale,
    colors = colors, cexCol = cexCol, cexRow = cexRow, revC = FALSE, labRow = rownames(plotmatrix),
    col_side_colors = col_side_colors, col_side_palette = col_side_palette, plot_method = "plotly",
    subplot_heights = subplot_heights, subplot_margin = 0.01, grid_gap = 1, hide_colorbar = hide_colorbar,
    cellnote = if (display_numbers) round(plotmatrix, 2) else NULL, draw_cellnote = display_numbers, ...
  )

  # heatmaply's branches_lwd argument only takes effect for plot_method = "ggplot";
  # for "plotly" the dendrogram is drawn via plotly::add_segments() with no line
  # width control at all, which renders as an inconsistent default thickness
  for (i in seq_along(p$x$data)) {
    if (identical(p$x$data[[i]]$mode, "lines")) {
      p$x$data[[i]]$line$width <- 1
    }
  }

  if (has_col_annotation) {
    p <- splitAnnotationLegend(p, col_side_colors, col_side_palette, ncol(plotmatrix))
  }

  p
}

#' Build one shared color palette across every value in an annotation data frame
#'
#' heatmaply's \code{col_side_colors}/\code{row_side_colors} mechanism encodes
#' the values of all annotation variables onto a single combined color scale,
#' so a single palette covering every unique value across every column is
#' needed, rather than one palette per column.
#'
#' @param sample_annotation A data frame with sample metadata
#'
#' @return output A named vector of colors, one per unique value across all
#'   columns of \code{sample_annotation}
#'

combinedAnnotationColors <- function(sample_annotation) {
  levels_all <- unique(unlist(lapply(sample_annotation, function(x) levels(factor(x)))))
  stats::setNames(makeColorScale(length(levels_all)), levels_all)
}

#' Replace heatmaply's combined annotation legend with one split by variable
#'
#' By default heatmaply shows a single generically-titled legend/colorbar
#' covering every value of every \code{col_side_colors} variable pooled
#' together. This hides that combined colorbar and adds one invisible,
#' legend-only trace per value of each annotation variable instead, grouped
#' and titled by variable name, using the same colors as the annotation bars.
#'
#' @param p A plotly htmlwidget as produced by \code{heatmaply::heatmaply()}
#'   with \code{col_side_colors} supplied
#' @param col_side_colors A data frame with sample metadata, as passed to
#'   \code{heatmaply::heatmaply()}
#' @param palette A named vector of colors as produced by
#'   \code{\link{combinedAnnotationColors}}
#' @param ncol_heatmap Number of columns in the main heatmap data, used to
#'   identify the annotation strip trace(s) among the plot's other traces
#'
#' @return output The modified plotly htmlwidget
#'

splitAnnotationLegend <- function(p, col_side_colors, palette, ncol_heatmap) {
  # Identify the traces that make up the heatmap (main data plus one strip per
  # side of annotation columns): all share ncol_heatmap columns. Among those,
  # the main data trace has the most rows - a more reliable signal than
  # matching row counts against ncol(col_side_colors), which could coincide
  # with the main trace's row count for a small enough gene selection.
  heatmap_trace_indices <- which(vapply(p$x$data, function(trace) {
    identical(trace$type, "heatmap") && !is.null(trace$z) && ncol(trace$z) == ncol_heatmap
  }, logical(1)))

  row_counts <- vapply(heatmap_trace_indices, function(i) nrow(p$x$data[[i]]$z), numeric(1))
  main_index <- heatmap_trace_indices[which.max(row_counts)]
  annotation_indices <- setdiff(heatmap_trace_indices, main_index)

  for (i in annotation_indices) {
    p$x$data[[i]]$showscale <- FALSE
  }

  heatmap_axes <- list(xaxis = p$x$data[[main_index]]$xaxis, yaxis = p$x$data[[main_index]]$yaxis)

  legend_traces <- list()
  for (variable in colnames(col_side_colors)) {
    for (level in levels(factor(col_side_colors[[variable]]))) {
      legend_traces[[length(legend_traces) + 1]] <- list(
        x = list(NULL), y = list(NULL), type = "scatter", mode = "markers",
        marker = list(color = palette[[level]], size = 10),
        name = level, legendgroup = variable,
        legendgrouptitle = list(text = variable),
        showlegend = TRUE, hoverinfo = "skip",
        xaxis = heatmap_axes$xaxis, yaxis = heatmap_axes$yaxis
      )
    }
  }
  p$x$data <- c(p$x$data, legend_traces)

  p
}

#' Generate a matrix of anova values for associating principal components with
#' categorical covariates.
#'
#' @param pca_coords Data frame of PCA coordinates, with samples by row and
#'   components by column.
#' @param pcameta Data frame of sample metadata with sample identifiers by row
#'   and variables by column.
#' @param fraction_explained Numeric vector containing the percent contribution
#'   to variance of each component
#'
#' @return output A numeric matrix of p values
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(90), nrow = 15, ncol = 6,
#'   dimnames = list(paste0("gene", 1:15), paste0("s", 1:6)))
#' pca <- compilePCAData(mat)
#' pcameta <- data.frame(
#'   condition = rep(c("treated", "control"), each = 3),
#'   batch = rep(c("A", "B", "C"), 2),
#'   row.names = colnames(mat)
#' )
#' anova_pca_metadata(pca$coords, pcameta, pca$percentVar)
#'
anova_pca_metadata <- function(pca_coords, pcameta, fraction_explained) {
  # Use 10 components or however many fewer is produced by the PCA

  last_pc <- 10
  if (ncol(pca_coords) < last_pc) {
    last_pc <- ncol(pca_coords)
  }

  pcameta <- pcameta[, chooseGroupingVariables(pcameta), drop = FALSE]

  # Make a blank matrix to hold the p values

  pvals <-
    matrix(
      data = NA,
      nrow = ncol(pcameta),
      ncol = last_pc,
      dimnames = list(
        colnames(pcameta),
        paste(
          paste("PC", seq_len(last_pc), sep = ""),
          " (",
          fraction_explained[seq_len(last_pc)],
          "%)",
          sep = ""
        )
      )
    )

  # Fill the matrix with anova p values

  for (i in seq_len(ncol(pcameta))) {
    for (j in seq_len(last_pc)) {
      fit <- aov(pca_coords[, j] ~ factor(pcameta[, i]))
      if ("Pr(>F)" %in% names(summary(fit)[[1]])) {
        pvals[i, j] <- summary(fit)[[1]][["Pr(>F)"]][[1]]
      }
    }
  }

  pvals
}

#' Make a PCA-vs-metadata association heatmap with \code{heatmaply()}
#'
#' Runs \code{\link{anova_pca_metadata}} on the supplied PCA coordinates and
#' sample metadata, then renders the resulting p value matrix with
#' \code{\link{interactiveHeatmap}}: -log10(p) sets the cell color, the raw p
#' values are shown on hover, and variables (rows) with a single, uninformative
#' value across all shown cells are dropped when \code{cluster_rows} is TRUE.
#'
#' @param pca_coords Data frame of PCA coordinates, with samples by row and
#'   components by column (e.g. the \code{x} element of \code{\link{runPCA}}'s
#'   \code{prcomp} result).
#' @param pcameta Data frame of sample metadata with sample identifiers by row
#'   and variables by column.
#' @param fraction_explained Numeric vector containing the percent contribution
#'   to variance of each component (e.g. from
#'   \code{\link{calculatePCAFractionExplained}}).
#' @param cluster_rows Cluster variables (rows) by their p value profile
#'   across components?
#' @param plot_height The total rendered height of the plot in pixels, passed
#'   through to \code{\link{interactiveHeatmap}}. Defaults to a height scaled
#'   to the number of variables.
#' @param ... Additional arguments passed to \code{\link{interactiveHeatmap}}
#'
#' @return output A plotly htmlwidget as produced by \code{\link{interactiveHeatmap}}
#'
#' @export
#'
#' @examples
#' pcameta <- data.frame(
#'   row.names = paste0("sample", 1:6),
#'   treatment = rep(c("control", "treated"), each = 3),
#'   batch = rep(c("a", "b"), 3)
#' )
#' pca_coords <- matrix(rnorm(6 * 4), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:4)))
#'
#' plotly_pca_metadata_heatmap(pca_coords, pcameta, fraction_explained = c(45, 25, 20, 10))
#'
plotly_pca_metadata_heatmap <- function(pca_coords, pcameta, fraction_explained, cluster_rows = TRUE, plot_height = NULL, ...) {
  pvals <- anova_pca_metadata(pca_coords = pca_coords, pcameta = pcameta, fraction_explained = fraction_explained)

  if (cluster_rows) {
    pvals <- pvals[rowsWithMultipleValues(pvals), , drop = FALSE]
  }

  plotmatrix <- pvals
  plotmatrix[plotmatrix < 0.001] <- 0.001
  plotmatrix <- log10(plotmatrix[!matrixStats::rowAlls(is.na(plotmatrix)), , drop = FALSE])

  displaymatrix <- pvals[rownames(plotmatrix), , drop = FALSE]

  if (is.null(plot_height)) {
    plot_height <- (nrow(plotmatrix) * 20) + 150
  }

  interactiveHeatmap(
    plotmatrix = plotmatrix, displaymatrix = displaymatrix, sample_annotation = NULL,
    cluster_rows = cluster_rows, cluster_cols = FALSE, scale = "none",
    row_labels = rownames(plotmatrix), colors = rev(viridisLite::viridis(100)),
    cexCol = 1, cexRow = 1, display_numbers = FALSE, hide_colorbar = TRUE,
    plot_height = plot_height, ...
  )
}
