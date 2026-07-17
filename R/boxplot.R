boxplot_modal <- list(id = "boxplot", title = "Value distributions")

#' The input function of the boxplot module
#'
#' This module produces displays of the distributions of the values in the
#' selected assay matrix. For low sample numbers (<= 20) the default is a
#' boxplot produced using \code{ggplot2}. For higher sample numbers, the default is
#' a line-based alternative using \code{plotly}.
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
#' boxplotInput("boxplot", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("boxplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
boxplotInput <- function(id, eselist) {
  ns <- NS(id)

  default_type <- "boxes"
  if (ncol(eselist[[1]]) > 50) {
    default_type <- "lines"
  }

  expression_filters <- selectmatrixInput(ns("sampleBoxplot"), eselist)
  distribution_plot_filters <- list(radioButtons(ns("plotType"), "Plot type", c("boxes", "lines", "density"), selected = default_type), numericInput(ns("whiskerDistance"),
    "Whisker distance in multiples of IQR",
    value = 1.5
  ), groupbyInput(ns("boxplot")))

  field_sets <- list()
  naked_fields <- list() # Things we don't want to wrap in a field set - probably hidden stuff

  # Don't create an empty field set if we're not grouping

  if (has_slot_data(eselist, "group_vars")) {
    field_sets$distribution_plot_filters <- distribution_plot_filters
  } else {
    naked_fields[[1]] <- distribution_plot_filters
  }

  field_sets <- c(field_sets, list(expression = expression_filters))

  list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The output function of the boxplot module
#'
#' This module produces displays of the distributions of the values in the
#' selected assay matrix. For low sample numbers (<= 20) the default is a
#' boxplot produced using \code{ggplot2}. For higher sample numbers, the default is
#' a line-based alternative using \code{plotly}.
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' boxplotOutput("boxplot")
#'
#' # Almost certainly used via application creation
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("boxplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
boxplotOutput <- function(id) {
  ns <- NS(id)
  list(
    modalInput(ns(boxplot_modal$id), "help", "help"),
    h3("Value distributions"), uiOutput(ns("quartilesPlot"))
  )
}

#' The server function of the boxplot module
#'
#' This module produces displays of the quartiles of the values in the
#' selected assay matrix. For low sample numbers (<= 20) the default is a
#' boxplot produced using \code{ggplot2}. For higher sample numbers, the default is
#' a line-based alternative using \code{plotly}.
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
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   boxplot("boxplot", eselist)
#'   app <- prepareApp("boxplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
boxplot <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(boxplot_modal$id, boxplot_modal$title)

    # Get the expression matrix - no need for a gene selection

    selectmatrix_reactives <- selectmatrix("sampleBoxplot", eselist, select_genes = FALSE)
    groupby_reactives <- groupby("boxplot", eselist = eselist, group_label = "Color by", selectColData = selectmatrix_reactives$selectColData)

    # Render the plot

    output$quartilesPlot <- renderUI({
      ns <- session$ns
      if (input$plotType == "boxes") {
        plotlyOutput(ns("sampleBoxplot"), height = "600px")
      } else if (input$plotType == "density") {
        plotlyOutput(ns("densityPlotly"), height = "600px")
      } else {
        plotlyOutput(ns("quartilesPlotly"), height = "600px")
      }
    })

    output$quartilesPlotly <- renderPlotly({
      selected_matrix <- selectmatrix_reactives$selectMatrix()
      ese <- selectmatrix_reactives$getExperiment()
      plotly_quartiles(selected_matrix, idToLabel(rownames(selected_matrix), ese), selectmatrix_reactives$getAssayMeasure(), whisker_distance = input$whiskerDistance)
    })

    output$densityPlotly <- renderPlotly({
      plotly_densityplot(selectmatrix_reactives$selectMatrix(), selectmatrix_reactives$selectColData(), groupby_reactives$getGroupby(), expressiontype = selectmatrix_reactives$getAssayMeasure(), palette = groupby_reactives$getPalette())
    })

    plot_source <- session$ns("sampleBoxplot")

    getLevels <- reactive({
      groupLevels(selectmatrix_reactives$selectColData(), groupby_reactives$getGroupby())
    })

    # Box traces come first (one per group, in getLevels() order) with no
    # leading trace, so a restyled trace index maps directly onto a group.
    hiddenGroups <- legendHiddenGroups(plot_source, getLevels, groupby_reactives$getGroupby, trace_offset = 0L)

    output$sampleBoxplot <- renderPlotly({
      withProgress(message = "Making sample boxplot", value = 0, {
        plotly_boxplot(selectmatrix_reactives$selectMatrix(), selectmatrix_reactives$selectColData(), groupby_reactives$getGroupby(),
          expressiontype = selectmatrix_reactives$getAssayMeasure(), whisker_distance = input$whiskerDistance,
          palette = groupby_reactives$getPalette(), hidden_groups = hiddenGroups(), source = plot_source
        )
      })
    })
  })
}

#' Make a boxplot with coloring by experimental variable
#'
#' A simple function using \code{ggplot2} to make a sample boxplot
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#' @param whisker_distance Passed to \code{\link[ggplot2]{geom_boxplot}} as
#' \code{coef}, controlling the length of the whiskers. See documentation of
#' that function for more info (default: 1.5).
#' @param base_size Passed to ggplot's \code{theme()}
#' @param palette_name Valid R color palette name
#' @param annotate_samples Add a suffix to sample labels reflecting their group?
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#'
#' @return output A \code{ggplot} output
#'
#' @keywords keywords
#'
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @export
#'
#' @examples
#' require(airway)
#' data(airway, package = "airway")
#' ggplot_boxplot(assays(airway)[[1]], data.frame(colData(airway)), colorby = "dex")
#'
ggplot_boxplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression", whisker_distance = 1.5, base_size = 11, palette_name = "Set1", annotate_samples = FALSE, should_transform = NULL) {
  plotdata <- ggplotify(plotmatrices, experiment, colorby, annotate_samples, should_transform = should_transform)

  if (!is.null(colorby)) {
    ncats <- length(unique(experiment[[colorby]]))
    if (is.null(palette)) {
      palette <- makeColorScale(ncats, palette = palette_name)
    }

    p <- ggplot(plotdata, aes(name, value, fill = colorby)) +
      geom_boxplot(coef = whisker_distance) +
      scale_fill_manual(name = prettifyVariablename(colorby), values = palette) +
      guides(fill = guide_legend(nrow = ceiling(ncats / 2)))
  } else {
    p <- ggplot(plotdata, aes(name, value)) +
      geom_boxplot()
  }

  if (is.list(plotmatrices) && length(plotmatrices) > 1) {
    n_col <- ifelse(sum(unlist(lapply(plotmatrices, ncol))) < 20, length(plotmatrices), 1)
    p <- p + facet_wrap(~type, ncol = n_col)
  }

  p <- p + theme_bw(base_size = base_size) + theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)), axis.title.x = element_blank(), legend.position = "bottom",
    axis.text.y = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.2)), title = element_text(size = rel(1.3)),
    strip.text.x = element_text(size = 10)
  ) + ylab(splitStringToFixedwidthLines(paste0(
    "log2(",
    prettifyVariablename(expressiontype), ")"
  ), 15))
}

#' Summarise a vector into the statistics a box plot needs
#'
#' Computes the quartiles, Tukey-style whisker extents (the most extreme
#' observations still within \code{whisker_distance} IQRs of the box), and the
#' outliers lying beyond them. This is the server-side reduction that lets
#' \code{\link{plotly_boxplot}} draw genuine box glyphs while sending only a
#' handful of numbers per box to the browser rather than every observation.
#'
#' @param values Numeric vector
#' @param labels Character vector of point labels, parallel to \code{values}
#' @param whisker_distance IQR multiplier for the whiskers (see \code{coef} in
#' \code{\link[ggplot2]{geom_boxplot}})
#'
#' @return A list with scalar \code{q1}, \code{median}, \code{q3},
#' \code{lowerfence} and \code{upperfence}, plus \code{outlier_values} and
#' \code{outlier_labels} vectors
#'
#' @keywords internal
box_summary <- function(values, labels, whisker_distance = 1.5) {
  keep <- is.finite(values)
  values <- values[keep]
  labels <- labels[keep]

  if (length(values) == 0) {
    return(list(
      q1 = NA_real_, median = NA_real_, q3 = NA_real_, lowerfence = NA_real_,
      upperfence = NA_real_, outlier_values = numeric(0), outlier_labels = character(0)
    ))
  }

  q <- quantile(values, probs = c(0.25, 0.5, 0.75), names = FALSE)
  iqr <- q[3] - q[1]
  lower_bound <- q[1] - whisker_distance * iqr
  upper_bound <- q[3] + whisker_distance * iqr
  within <- values >= lower_bound & values <= upper_bound

  list(
    q1 = q[1], median = q[2], q3 = q[3],
    lowerfence = if (any(within)) min(values[within]) else q[1],
    upperfence = if (any(within)) max(values[within]) else q[3],
    outlier_values = values[!within],
    outlier_labels = labels[!within]
  )
}

#' Make an interactive boxplot with coloring by experimental variable
#'
#' Draws a \code{plotly} box plot of the value distribution in each sample.
#' Box statistics (quartiles, whiskers, outliers) are computed server-side and
#' supplied to plotly as precomputed values, so the browser payload scales with
#' the number of samples rather than the size of the matrix. This makes it
#' usable on full expression matrices where sending every point would not be.
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#' @param palette_name Valid R color palette name
#' @param whisker_distance IQR multiplier for the whiskers, and the boundary
#' beyond which points are drawn as outliers (see \code{coef} in
#' \code{\link[ggplot2]{geom_boxplot}}, default: 1.5)
#' @param annotate_samples Add a suffix to sample labels reflecting their group?
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#' @param max_outliers Maximum number of outlier points to draw per facet. When
#' a sample set produces more, the most extreme (furthest from the median) are
#' kept so the overlay stays bounded (default: 500).
#' @param hidden_groups Values of \code{colorby} to exclude: their samples are
#'   dropped so the plot is redrawn on the remainder, while they stay in the
#'   legend (as \code{legendonly}) so they can be toggled back on.
#' @param source Optional plotly source id used to route legend-click
#'   (\code{plotly_restyle}) events back to a Shiny session.
#'
#' @export
#' @return output A \code{plotly} output
#'
#' @keywords keywords
#'
#' @examples
#' require(airway)
#' data(airway, package = "airway")
#' plotly_boxplot(assays(airway)[[1]], data.frame(colData(airway)), colorby = "dex")
#'
plotly_boxplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression", palette_name = "Set1", whisker_distance = 1.5, annotate_samples = FALSE, should_transform = NULL, max_outliers = 500, hidden_groups = character(0), source = NULL) {
  if (!is.list(plotmatrices)) {
    plotmatrices <- list(" " = plotmatrices)
  }

  # Order samples so members of the same group sit together, preserving
  # first-seen order of both samples and groups (mirrors ggplotify()).

  if (!is.null(colorby)) {
    groups <- na.replace(as.character(experiment[[colorby]]), "N/A")
  } else {
    groups <- rep(" ", nrow(experiment))
  }
  group_levels <- unique(groups)
  sample_order <- order(factor(groups, levels = group_levels))
  samples <- rownames(experiment)[sample_order]
  groups <- groups[sample_order]

  palette <- resolvePalette(palette, group_levels, palette_name)

  axis_labels <- samples
  if (annotate_samples && !is.null(colorby)) {
    axis_labels <- paste0(samples, " (", groups, ")")
  }
  names(axis_labels) <- samples

  visible_mask <- !(groups %in% hidden_groups)
  visible_samples <- samples[visible_mask]

  # Legend-click events are only routed for a single (non-subplotted) panel,
  # which is what the Shiny module produces.
  event_source <- if (length(plotmatrices) == 1) source else NULL

  facet_names <- prettifyVariablename(names(plotmatrices))
  yaxis_title <- splitStringToFixedwidthLines(paste0("log2(", prettifyVariablename(expressiontype), ")"), 15)

  facet_plots <- lapply(seq_along(plotmatrices), function(i) {
    m <- cond_log2_transform_matrix(as.matrix(plotmatrices[[i]]), should_transform = should_transform, rmzeros = TRUE)
    m <- m[, samples, drop = FALSE]

    stats <- lapply(samples, function(s) box_summary(m[, s], rownames(m), whisker_distance))
    names(stats) <- samples

    p <- if (is.null(event_source)) plot_ly() else plot_ly(source = event_source)

    for (g in group_levels) {
      grp_samples <- samples[groups == g]
      s_stats <- stats[grp_samples]
      p <- add_trace(
        p,
        type = "box",
        x = unname(axis_labels[grp_samples]),
        lowerfence = vapply(s_stats, function(x) x$lowerfence, numeric(1)),
        q1 = vapply(s_stats, function(x) x$q1, numeric(1)),
        median = vapply(s_stats, function(x) x$median, numeric(1)),
        q3 = vapply(s_stats, function(x) x$q3, numeric(1)),
        upperfence = vapply(s_stats, function(x) x$upperfence, numeric(1)),
        name = g,
        legendgroup = g,
        fillcolor = palette[[g]],
        line = list(color = "black"),
        visible = if (g %in% hidden_groups) "legendonly" else TRUE,
        showlegend = !is.null(colorby) && i == 1
      )
    }

    outliers <- do.call(rbind, lapply(visible_samples, function(s) {
      ov <- stats[[s]]$outlier_values
      if (length(ov) == 0) {
        return(NULL)
      }
      data.frame(
        x = unname(axis_labels[s]), y = ov, label = stats[[s]]$outlier_labels,
        deviation = abs(ov - stats[[s]]$median), stringsAsFactors = FALSE
      )
    }))

    if (!is.null(outliers) && nrow(outliers) > 0) {
      if (nrow(outliers) > max_outliers) {
        outliers <- outliers[order(outliers$deviation, decreasing = TRUE)[seq_len(max_outliers)], ]
      }
      p <- add_trace(
        p,
        type = "scatter", mode = "markers",
        x = outliers$x, y = outliers$y,
        text = outliers$label, hoverinfo = "text",
        marker = list(color = "black", size = 4, opacity = 0.5),
        name = "outliers", showlegend = FALSE
      )
    }

    xaxis <- list(title = if (length(plotmatrices) > 1) facet_names[i] else NULL)
    if (length(visible_samples) > 0) {
      xaxis$categoryorder <- "array"
      xaxis$categoryarray <- unname(axis_labels[visible_samples])
    }

    p %>%
      layout(
        xaxis = xaxis,
        yaxis = list(title = yaxis_title, zeroline = FALSE)
      )
  })

  if (length(facet_plots) == 1) {
    p <- facet_plots[[1]]
  } else {
    p <- subplot(facet_plots, nrows = length(facet_plots), shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
  }

  p <- p %>%
    layout(
      legend = list(
        title = list(text = if (!is.null(colorby)) prettifyVariablename(colorby) else ""),
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.3
      ),
      margin = list(b = 120),
      hovermode = "closest"
    )

  if (!is.null(event_source)) {
    p <- event_register(p, "plotly_restyle")
  }
  p
}

#' Make a static density plot with ggplot2
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how lines should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#' @param base_size Passed to ggplot's \code{theme()}
#' @param palette_name Valid R color palette name
#' @param annotate_samples Add a suffix to sample labels reflecting their group?
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#'
#' @export
#'
#' @return output A \code{ggplot} output

ggplot_densityplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression", base_size = 16, palette_name = "Set1", annotate_samples = FALSE, should_transform = NULL) {
  plotdata <- ggplotify(plotmatrices, experiment, colorby, value_type = "density", annotate_samples = annotate_samples, should_transform = should_transform)
  if (is.null(palette)) {
    ncats <- length(unique(plotdata$colorby))
    palette <- makeColorScale(ncats, palette = palette_name)
  }

  p <- ggplot(data = plotdata) +
    geom_area(aes(x = value, y = density, fill = colorby, color = colorby, group = name), alpha = 0.4) +
    scale_fill_manual(name = prettifyVariablename(colorby), values = palette) +
    scale_color_manual(name = prettifyVariablename(colorby), values = palette) +
    ylab("Density") +
    xlab(splitStringToFixedwidthLines(paste0(
      "log2(",
      prettifyVariablename(expressiontype), ")"
    ), 15)) +
    guides(fill = guide_legend(title = prettifyVariablename(colorby))) +
    theme(legend.position = "bottom")

  if (is.list(plotmatrices) && length(plotmatrices) > 1) {
    p <- p + facet_wrap(~type, ncol = 1, scales = "free_y")
  }

  p + theme_bw(base_size = base_size) + theme(
    legend.position = "bottom"
  )
}

#' Make a dynamic density plot with plotly
#'
#' A simple function using \code{plotly} to make a sample density plot.
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how lines should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#' @param palette_name Valid R color palette name
#' @param annotate_samples Add a suffix to sample labels reflecting their group?
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#'
#' @importFrom dplyr group_map
#' @export
#'
#' @return output A \code{plotly} output

plotly_densityplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression", palette_name = "Set1", annotate_samples = FALSE, should_transform = NULL) {
  plotdata <- ggplotify(plotmatrices, experiment, colorby, value_type = "density", annotate_samples = annotate_samples, should_transform = should_transform)
  if (is.null(palette)) {
    ncats <- length(unique(plotdata$colorby))
    palette <- makeColorScale(ncats, palette = palette_name)
  }

  plotdata %>%
    group_by(type) %>%
    group_map(
      ~ plot_ly(
        data = .,
        x = ~value,
        y = ~density,
        color = ~colorby,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        legendgroup = ~name,
        colors = palette,
        showlegend = (.y == "Raw"),
        alpha = 0.2
      ) %>%
        layout(
          hoverlabel = list(namelength = -1),
          xaxis = list(title = splitStringToFixedwidthLines(paste0(
            "log2(",
            prettifyVariablename(expressiontype), ")"
          ), 15)),
          yaxis = list(title = paste(.x$type[1])),
          legend = list(
            title = list(text = "Sample"),
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.2
          )
        ),
      .keep = TRUE
    ) %>%
    subplot(
      nrows = length(unique(plotdata$type)),
      shareX = TRUE,
      shareY = FALSE,
      titleX = TRUE,
      titleY = TRUE
    )
}

#' Make a line-based alternative to boxplots
#'
#' Box-plots become unmanagable with large numbers of samples. This function
#' plots lines at the median, quartiles, and whiskers, plotting points for
#' outliers beyond that
#'
#' @param matrix Numeric matrix
#' @param labels String vector of labels to be used for each matrix row
#' @param expressiontype Y axis label
#' @param whisker_distance IQR multiplier for whiskers, and beyond which to
#' show outliers (see \code{coef} in \code{\link[ggplot2]{geom_boxplot}})
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#'
#' @export
#' @examples
#' data(airway, package = "airway")
#' plotly_quartiles(assays(airway)[[1]], as(airway, "ExploratorySummarizedExperiment"))
#'
plotly_quartiles <- function(matrix, labels = rownames(matrix), expressiontype = "expression", whisker_distance = 1.5, should_transform = NULL) {
  matrix <- cond_log2_transform_matrix(matrix, should_transform = should_transform)

  quantiles <- apply(matrix, 2, quantile, na.rm = TRUE)
  samples <- structure(colnames(matrix), names = colnames(matrix))
  iqrs <- lapply(samples, function(x) {
    quantiles["75%", x] - quantiles["25%", x]
  })

  outliers <- lapply(samples, function(x) {
    y <- structure(matrix[, x], names = rownames(matrix))
    outlier_rows <- which(y > quantiles["75%", x] + iqrs[[x]] * whisker_distance | y < quantiles["25%", x] - iqrs[[x]] * whisker_distance)
    ol <- y[outlier_rows]
    if (length(ol) > 0) {
      data.frame(x = x, y = ol, label = labels[outlier_rows])
    } else {
      NULL
    }
  })
  outliers <- do.call(rbind, outliers[!unlist(lapply(outliers, is.null))])

  # These lines to force plotly to use and display sample IDs as strings. For some reason character strings of numeric things get converted back

  # The plotting business

  plot_ly(data.frame(quantiles), mode = "markers") %>%
    add_trace(
      x = outliers$x, y = outliers$y, name = "outliers", marker = list(color = "black"), hoverinfo = "text",
      text = outliers$label, type = "scatter"
    ) %>%
    add_lines(
      x = samples, y = quantiles["75%", ] + ((quantiles["75%", ] - quantiles["25%", ]) * whisker_distance),
      line = list(width = 1, color = "grey", dash = "dash"), name = paste0("75%<br />+ (IQR * ", whisker_distance, ")")
    ) %>%
    add_lines(x = samples, y = quantiles[
      "75%",
      samples
    ], line = list(dash = "dash", color = "black"), name = "75%") %>%
    add_lines(x = samples, y = quantiles["50%", samples], line = list(
      dash = "solid",
      color = "black"
    ), name = "median") %>%
    add_lines(x = samples, y = quantiles["25%", samples], line = list(dash = "longdash", color = "black"), name = "25%") %>%
    add_lines(x = samples, y = quantiles["25%", ] - ((quantiles["75%", ] - quantiles["25%", ]) * whisker_distance), line = list(
      width = 1, color = "grey",
      dash = "longdash"
    ), name = paste0("25%<br />- (IQR * ", whisker_distance, ")")) %>%
    layout(xaxis = list(
      title = NULL
    ), yaxis = list(
      title = paste0("log2(", expressiontype, ")"),
      zeroline = FALSE
    ), margin = list(b = 150), hovermode = "closest", title = NULL)
}
