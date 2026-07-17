#' The input function of the dendrogram module
#'
#' This module will produce a sample clustering dendrogram based on
#' user-selected parameters of row (e.g. gene) and column (sample) selection
#' provided by the \code{selectmatrix} module, as well distance matrix
#' generation and clustering method.
#'
#' This funcion provides the form elements to control the display
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
#' library(shinyngs)
#' data(zhangneurons)
#' dendroInput("myid", zhangneurons)
#'
dendroInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("dendro"), eselist)

  dendro_filters <- list(selectInput(ns("corMethod"), "Correlation method", c(Pearson = "pearson", Spearman = "spearman", Kendall = "kendall")), selectInput(
    ns("clusterMethod"),
    "Clustering method", c(
      `Ward minimum variance clustering` = "ward.D2", `Single linkage` = "single", `Complete linkage` = "complete", `Average linkage` = "average",
      WPGMA = "mcquittye", UPGMC = "centroid"
    )
  ), groupbyInput(ns("dendro")))

  fieldSets(ns("fieldset"), list(clustering = dendro_filters, expression = expression_filters))
}

#' The output function of the dendro module
#'
#' This module will produce a sample clustering dendrogram based on
#' user-selected parameters of row (e.g. gene) and column (sample) selection
#' provided by the \code{selectmatrix} module, as well distance matrix
#' generation and clustering method.
#'
#' This provides actual dendrogram plot element for display by applications
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' dendroOutput("myid")
#'
dendroOutput <- function(id) {
  ns <- NS(id)
  list(modalInput(ns("dendro"), "help", "help"), h3("Sample clustering dendrogram"), plotlyOutput(ns("sampleDendroPlot"), height = "480px"))
}

#' The server function of the dendrogram module
#'
#' This module will produce a sample clustering dendrogram based on
#' user-selected parameters of row (e.g. gene) and column (sample) selection
#' provided by the \code{selectmatrix} module, as well distance matrix
#' generation and clustering method.
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
#' dendro("myid", eselist)
#'
dendro <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer("dendro", "Sample clustering dendrogram")

    # Get the expression matrix - no need for a gene selection

    unpack.list(selectmatrix("dendro", eselist, select_genes = TRUE, var_n = 1000, provide_all_genes = TRUE, default_gene_select = "variance"))
    unpack.list(groupby("dendro", eselist = eselist, group_label = "Color by", selectColData = selectColData))

    plot_source <- session$ns("sampleDendroPlot")

    # Groups the user has toggled off via the legend; their samples are dropped
    # from the matrix so the tree is recomputed on the remainder.
    hiddenGroups <- reactiveVal(character(0))

    getLevels <- reactive({
      dendroGroupLevels(selectColData(), getGroupby())
    })

    # A change of grouping variable invalidates the toggled-off group names
    observeEvent(getGroupby(),
      {
        hiddenGroups(character(0))
      },
      ignoreNULL = FALSE
    )

    # Clicking a legend entry emits plotly_restyle with the trace's new
    # visibility; translate that into showing/hiding the group's samples. Trace 0
    # is the branch tree, so trace i corresponds to the i-th group.
    observeEvent(event_data("plotly_restyle", source = plot_source), {
      ed <- event_data("plotly_restyle", source = plot_source)
      visible <- ed[[1]][["visible"]]
      if (is.null(visible)) {
        return()
      }
      traces <- unlist(ed[[2]])
      levels_all <- getLevels()
      current <- hiddenGroups()
      for (j in seq_along(traces)) {
        trace <- traces[j]
        if (trace < 1 || trace > length(levels_all)) next
        level <- levels_all[trace]
        state <- if (length(visible) >= j) visible[[j]] else visible[[1]]
        current <- if (identical(state, "legendonly")) union(current, level) else setdiff(current, level)
      }
      hiddenGroups(current)
    })

    output$sampleDendroPlot <- renderPlotly({
      withProgress(message = "Making sample dendrogram", value = 0, {
        plotly_clusteringDendrogram(selectMatrix(), selectColData(), getGroupby(),
          cor_method = input$corMethod, cluster_method = input$clusterMethod, matrixTitle(),
          palette = getPalette(), hidden_groups = hiddenGroups(), source = plot_source
        )
      })
    })
  })
}

#' Make a clustering dendrogram with coloring by experimental variable
#'
#' A simple function using \code{ggdendro} to make a sample dendrogram
#'
#' @param plotmatrix Expression/ other data matrix
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param cor_method Correlation method, passed to cor() (default: pearson).
#' @param cluster_method Clustering method, passed to hclust() (default:
#' ward.D).
#' @param plot_title Plot title
#' @param labelspace Vertical fraction of plot to be used for labels (default: 0.2).
#' @param palette_name Valid R color palette name
#'
#' @return output A \code{ggplot} output
#'
#' @keywords keywords
#'
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @import ggdendro
#'
#' @export
#'
#' @examples
#' # Make a dendrogram with the data in airway
#'
#' require(airway)
#' data(airway, pakckage = "airway")
#' clusteringDendrogram(assays(airway)[[1]], data.frame(colData(airway)), colorby = "dex")
#'
#' # Do the same, but only usig the 1000 most variant rows and see how the
#' # clustering improves.
#'
#' mymatrix <- assays(airway)[[1]]
#' mymatrix <- mymatrix[order(apply(mymatrix, 1, var), decreasing = TRUE)[1:1000], ]
#' clusteringDendrogram(mymatrix, data.frame(colData(airway)), colorby = "dex")
#'
clusteringDendrogram <- function(plotmatrix, experiment, colorby = NULL, cor_method = "pearson", cluster_method = "ward.D", plot_title = "", labelspace = 0.2,
                                 palette = NULL, palette_name = "Set1") {
  plotmatrix <- log2(plotmatrix + 1)

  hcd <- calculateDendrogram(plotmatrix, cor_method, cluster_method)

  ddata_x <- ggdendro::dendro_data(hcd)

  p2 <- ggplot(ggdendro::segment(ddata_x)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend))

  labs <- ggdendro::label(ddata_x)

  ymax <- max(ddata_x$segments$yend)

  # Things are much simpler without coloring the samples

  if (is.null(colorby)) {
    p3 <- p2 + geom_text(data = labs, angle = 90, hjust = 1, size = rel(6), aes_string(label = "label", x = "x", y = -(ymax / 40)), show.legend = F)

    p3 <- p3 + ggdendro::theme_dendro() + ylim(-(ymax / 3), ymax)

    p3 <- p3 + geom_point(data = labs, aes_string(x = "x", y = 0), size = 4)
  } else {
    if (is.null(palette)) {
      palette <- makeColorScale(length(unique(experiment[[colorby]])), palette = palette_name)
    }
    labs[[colorby]] <- as.character(experiment[[colorby]][match(labs$label, rownames(experiment))])
    labs[[colorby]] <- na.replace(labs[[colorby]], replacement = "N/A")

    labs[[colorby]] <- factor(labs[[colorby]], levels = unique(na.replace(experiment[[colorby]], "N/A")))
    shapes <- rep(15:20, 10)[1:length(unique(experiment[[colorby]]))]

    p3 <- p2 +
      geom_text(
        data = labs,
        angle = 90,
        hjust = 1,
        size = rel(5),
        aes_string(label = "label", x = "x", y = -(ymax / 40), colour = as.name(colorby)),
        show.legend = F
      )

    total_axis_size <- ymax * (1 / (1 - labelspace))

    p3 <- p3 + ggdendro::theme_dendro() + ylim(-(total_axis_size * labelspace), ymax) + scale_color_manual(name = prettifyVariablename(colorby), values = palette)

    p3 <- p3 +
      geom_point(data = labs, aes_string(x = "x", y = 0, colour = as.name(colorby), shape = as.name(colorby)), size = 4) +
      scale_shape_manual(values = shapes, name = prettifyVariablename(colorby)) +
      theme(title = element_text(size = rel(1.8)), legend.text = element_text(size = rel(1.8))) +
      ggtitle(plot_title)
  }

  if (!is.null(colorby)) {
    p3 <- p3 + guides(color = guide_legend(nrow = ceiling(length(unique(experiment[[colorby]])) / 2)))
  }
  print(p3 + theme(title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.position = "bottom") + ggtitle(plot_title))
}

#' Group levels for a dendrogram, in the order legend traces are built
#'
#' Shared by \code{plotly_clusteringDendrogram()} and its Shiny module so the
#' trace order and the server's trace-index-to-group mapping cannot drift apart.
#'
#' @param experiment Sample annotation data frame
#' @param colorby Column name in \code{experiment}, or NULL for no grouping
#'
#' @return Character vector of unique group values, with \code{"N/A"} for missing
#'
#' @noRd
dendroGroupLevels <- function(experiment, colorby) {
  if (is.null(colorby)) {
    return(character(0))
  }
  unique(na.replace(as.character(experiment[[colorby]]), "N/A"))
}

#' Make an interactive clustering dendrogram colored by experimental variable
#'
#' A \code{plotly} counterpart to \code{clusteringDendrogram()}: the branch
#' geometry is derived from the same \code{hclust()} tree, and the leaves are
#' placed as markers colored by an experimental variable. Sample names sit on
#' the x-axis (rotated, with plotly reserving the label margin), and hovering a
#' leaf reveals the sample name and its group value.
#'
#' @param plotmatrix Expression/ other data matrix
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how leaves should be colored
#' @param cor_method Correlation method, passed to cor() (default: pearson).
#' @param cluster_method Clustering method, passed to hclust() (default: ward.D).
#' @param plot_title Plot title
#' @param palette Palette of colors, one for each unique value derived from
#'   \code{colorby}.
#' @param palette_name Valid R color palette name
#' @param hidden_groups Values of \code{colorby} to exclude: their samples are
#'   dropped and the tree is recomputed on the remainder, while the groups stay
#'   in the legend (as \code{legendonly}) so they can be toggled back on.
#' @param source A plotly event source string, used to route legend-click
#'   (\code{plotly_restyle}) events back to a Shiny session.
#'
#' @return output A \code{plotly} plot object
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' data(airway, package = "airway")
#' mymatrix <- assays(airway)[[1]]
#' mymatrix <- mymatrix[order(apply(mymatrix, 1, var), decreasing = TRUE)[1:1000], ]
#' plotly_clusteringDendrogram(mymatrix, data.frame(colData(airway)), colorby = "dex")
#'
plotly_clusteringDendrogram <- function(plotmatrix, experiment, colorby = NULL, cor_method = "pearson", cluster_method = "ward.D", plot_title = "",
                                        palette = NULL, palette_name = "Set1", hidden_groups = character(0), source = NULL) {
  plot_title <- gsub("\n", "<br>", plot_title, fixed = TRUE)

  # Group membership per sample, and the full set of groups. Both are derived
  # from the complete sample set so the palette, symbols and legend stay stable
  # as groups are toggled on and off.
  levels_all <- dendroGroupLevels(experiment, colorby)
  if (is.null(colorby)) {
    col_groups <- character(ncol(plotmatrix))
  } else {
    col_groups <- na.replace(as.character(experiment[[colorby]][match(colnames(plotmatrix), rownames(experiment))]), "N/A")
  }

  if (is.null(palette) || any(is.na(palette[seq_along(levels_all)]))) {
    palette <- makeColorScale(max(length(levels_all), 1), palette = palette_name)
  }
  palette <- stats::setNames(palette[seq_along(levels_all)], levels_all)
  symbols <- stats::setNames(rep(c("circle", "square", "diamond", "triangle-up", "cross", "x"), length.out = length(levels_all)), levels_all)

  visible_matrix <- plotmatrix[, !(col_groups %in% hidden_groups), drop = FALSE]

  p <- plotly::plot_ly(source = source)

  # Trace 0 is always the branch tree (empty when too few samples remain to
  # cluster), so a Shiny handler can map any later trace index onto levels_all.
  labs <- NULL
  ymax <- 1
  if (ncol(visible_matrix) >= 2) {
    hcd <- calculateDendrogram(log2(visible_matrix + 1), cor_method, cluster_method)
    ddata <- ggdendro::dendro_data(hcd)
    segs <- ggdendro::segment(ddata)
    labs <- ggdendro::label(ddata)
    ymax <- max(c(segs$y, segs$yend))
    p <- plotly::add_lines(p,
      x = as.vector(rbind(segs$x, segs$xend, NA)), y = as.vector(rbind(segs$y, segs$yend, NA)),
      line = list(color = "black", width = 1), hoverinfo = "none", showlegend = FALSE
    )
  } else {
    p <- plotly::add_lines(p, x = numeric(0), y = numeric(0), hoverinfo = "none", showlegend = FALSE)
  }

  if (is.null(colorby)) {
    if (!is.null(labs)) {
      p <- plotly::add_markers(p, x = labs$x, y = rep(0, nrow(labs)), text = labs$label, hoverinfo = "text", marker = list(color = "black", size = 8), showlegend = FALSE)
    }
  } else {
    leaf_group <- if (is.null(labs)) character(0) else na.replace(as.character(experiment[[colorby]][match(labs$label, rownames(experiment))]), "N/A")
    # One trace per group, in levels_all order, so a Shiny handler can map a
    # restyled trace index onto a group.
    for (level in levels_all) {
      keep <- leaf_group == level
      marker <- list(color = palette[[level]], symbol = symbols[[level]], size = 9, line = list(color = "black", width = 0.5))
      if (any(keep)) {
        p <- plotly::add_markers(p,
          x = labs$x[keep], y = rep(0, sum(keep)),
          text = paste0(labs$label[keep], "<br>", prettifyVariablename(colorby), ": ", level), hoverinfo = "text", marker = marker, name = level
        )
      } else {
        # No leaves to place, but keep a clickable legend entry: dimmed
        # (legendonly) when the user hid the group, otherwise a normal entry
        # drawing nothing (an NA point is not plotted).
        hidden <- level %in% hidden_groups
        p <- plotly::add_markers(p,
          x = if (hidden) 0 else NA_real_, y = if (hidden) 0 else NA_real_,
          marker = marker, name = level, hoverinfo = "skip", visible = if (hidden) "legendonly" else TRUE
        )
      }
    }
  }

  # Leaves sit at y = 0 (the tree tips); floor the axis just below 0 so the
  # rotated sample labels hang beneath the markers rather than behind them.
  leaf_x <- if (is.null(labs)) numeric(0) else labs$x
  xaxis <- list(
    title = "", tickmode = "array", tickvals = leaf_x,
    ticktext = if (is.null(labs)) character(0) else labs$label, tickangle = -90,
    automargin = TRUE, zeroline = FALSE, showgrid = FALSE,
    range = if (length(leaf_x)) c(min(leaf_x) - 0.5, max(leaf_x) + 0.5) else c(-0.5, 0.5)
  )
  yaxis <- list(title = "Height", zeroline = FALSE, showgrid = FALSE, range = c(-ymax * 0.05, ymax * 1.05))

  annotations <- NULL
  if (ncol(visible_matrix) < 2) {
    annotations <- list(list(text = "Select at least two groups to cluster", xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
  }

  p <- plotly::layout(p,
    title = plot_title, xaxis = xaxis, yaxis = yaxis, hovermode = "closest", annotations = annotations,
    legend = list(title = list(text = if (is.null(colorby)) "" else prettifyVariablename(colorby)))
  )

  if (!is.null(source)) {
    p <- plotly::event_register(p, "plotly_restyle")
  }
  p
}

#' Calculate a distance matrix based on correlation
#'
#' @param plotmatrix Expression/ other data matrix
#' @param cor_method 'spearman' or 'perason'
#'
#' @return output Object of class 'dist'
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' data(airway, package = "airway")
#' mymatrix <- assays(airway)[[1]]
#' calculateDist(mymatrix)
#'
calculateDist <- function(plotmatrix, cor_method = "spearman") {
  as.dist(1 - cor(plotmatrix, method = cor_method))
}

#' Calculate a clustering dendgrogram based on correlation
#'
#' @param plotmatrix Expression/ other data matrix
#' @param cor_method 'spearman' or 'perason'
#' @param cluster_method Clustering method to pass to hclust (Default: 'ward.D2')
#'
#' @return output Object of class 'dist'
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' data(airway, package = "airway")
#' mymatrix <- assays(airway)[[1]]
#' calculateDendrogram(mymatrix)
#'
calculateDendrogram <- function(plotmatrix, cor_method = "spearman", cluster_method = "ward.D2") {
  dd <- calculateDist(plotmatrix, cor_method = cor_method)

  hc <- hclust(dd, method = cluster_method)

  as.dendrogram(hc)
}
