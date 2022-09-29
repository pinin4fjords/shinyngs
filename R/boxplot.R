#' The input function of the boxplot module
#'
#' This module produces displays of the quartiles of the values in the
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
#' boxplotInput(ns("boxplot"), eselist)
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("boxplot", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
boxplotInput <- function(id, eselist) {
  ns <- NS(id)

  default_type <- "boxes"
  if (ncol(eselist[[1]]) > 50) {
    default_type <- "lines"
  }

  expression_filters <- selectmatrixInput(ns("sampleBoxplot"), eselist)
  quartile_plot_filters <- list(radioButtons(ns("plotType"), "Plot type", c("boxes", "lines"), selected = default_type), numericInput(ns("whiskerDistance"),
    "Whisker distance in multiples of IQR",
    value = 1.5
  ), groupbyInput(ns("boxplot")))

  field_sets <- list()
  naked_fields <- list() # Things we don't want to wrap in a field set - probably hidden stuff

  # Don't create an empty field set if we're not grouping

  if (length(eselist@group_vars) > 0) {
    field_sets$quartile_plot_filters <- quartile_plot_filters
  } else {
    naked_fields[[1]] <- quartile_plot_filters
  }

  field_sets <- c(field_sets, list(expression = expression_filters, export = plotdownloadInput(ns("boxplot"), "box plot")))

  list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The output function of the boxplot module
#'
#' This module produces displays of the quartiles of the values in the
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
#' data(zhangneurons)
#' app <- prepareApp("boxplot", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
boxplotOutput <- function(id) {
  ns <- NS(id)
  list(
    modalInput(ns("boxplot"), "help", "help"), modalOutput(ns("boxplot"), "Quartile plots", includeMarkdown(system.file("inlinehelp", "boxplot.md", package = packageName()))),
    h3("Quartile plots"), uiOutput(ns("quartilesPlot"))
  )
}

#' The server function of the boxplot module
#'
#' This module produces displays of the quartiles of the values in the
#' selected assay matrix. For low sample numbers (<= 20) the default is a
#' boxplot produced using \code{ggplot2}. For higher sample numbers, the default is
#' a line-based alternative using \code{plotly}.
#'
#' This function is not called directly, but rather via callModule() (see
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' callModule(boxplot, "boxplot", eselist)
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("boxplot", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
boxplot <- function(input, output, session, eselist) {

  # Get the expression matrix - no need for a gene selection

  unpack.list(callModule(selectmatrix, "sampleBoxplot", eselist, select_genes = FALSE))
  unpack.list(callModule(groupby, "boxplot", eselist = eselist, group_label = "Color by", selectColData = selectColData))

  # Render the plot

  output$quartilesPlot <- renderUI({
    ns <- session$ns
    if (input$plotType == "boxes") {
      plotOutput(ns("sampleBoxplot"))
    } else {
      plotlyOutput(ns("quartilesPlotly"), height = "600px")
    }
  })

  output$quartilesPlotly <- renderPlotly({
    plotly_quartiles(selectMatrix(), getExperiment(), getAssayMeasure(), whisker_distance = input$whiskerDistance)
  })

  output$sampleBoxplot <- renderPlot(
    {
      withProgress(message = "Making sample boxplot", value = 0, {
        ggplot_boxplot(selectMatrix(), selectColData(), getGroupby(), expressiontype = getAssayMeasure(), whisker_distance = input$whiskerDistance, palette = getPalette())
      })
    },
    height = 600
  )

  # Provide the plot for download

  plotSampleBoxplot <- reactive({
    ggplot_boxplot(selectMatrix(), selectColData(), colorBy())
  })

  # Call to plotdownload module

  callModule(plotdownload, "boxplot", makePlot = plotSampleBoxplot, filename = "boxplot.png", plotHeight = 600, plotWidth = 800)
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
ggplot_boxplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression", whisker_distance = 1.5) {
  plotdata <- ggplotify(plotmatrices, experiment, colorby)

  if (!is.null(colorby)) {
    ncats <- length(unique(experiment[[colorby]]))
    if (is.null(palette)) {
      palette <- makeColorScale(ncats)
    }

    p <- ggplot(plotdata, aes(name, log2_count, fill = colorby)) +
      geom_boxplot(coef = whisker_distance) +
      scale_fill_manual(name = prettifyVariablename(colorby), values = palette) +
      guides(fill = guide_legend(nrow = ceiling(ncats / 2)))
  } else {
    p <- ggplot(plotdata, aes(name, log2_count)) +
      geom_boxplot()
  }

  if (is.list(plotmatrices) && length(plotmatrices) > 1) {
    p <- p + facet_wrap(~type)
  }

  p <- p + theme_bw() + theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)), axis.title.x = element_blank(), legend.position = "bottom",
    axis.text.y = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.2)), title = element_text(size = rel(1.3))
  ) + ylab(splitStringToFixedwidthLines(paste0(
    "log2(",
    expressiontype, ")"
  ), 15))

  print(p)
}

#' Make a boxplot with coloring by experimental variable
#'
#' A simple function using \code{plotly} to make a sample boxplot.
#' NOT CURRENTLY USED DUE TO RESOURCE REQUIREMENTS ON LARGE MATRICES
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#'
#' @export
#' @return output A \code{plotly} output
#'
#' @keywords keywords

plotly_boxplot <- function(plotmatrices, experiment, colorby, palette = NULL, expressiontype = "expression") {
  plotdata <- ggplotify(plotmatrices, experiment, colorby)

  ncats <- length(unique(plotdata$colorby))
  if (is.null(palette)) {
    palette <- makeColorScale(ncats)
  }

  plotdata %>%
    group_by(type) %>%
    group_map(
      ~ plot_ly(
        data = .,
        x = ~name,
        y = ~log2_count,
        color = ~colorby,
        text = ~gene,
        type = "box",
        legendgroup = ~colorby,
        colors = palette,
        showlegend = (.y == "Raw")
      ) %>%
        layout(
          xaxis = list(title = paste(.x$type[1])),
          yaxis = list(title = splitStringToFixedwidthLines(paste0(
            "log2(",
            expressiontype, ")"
          ), 15)),
          legend = list(
            title = list(text = prettifyVariablename(colorby)),
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.3
          )
        ) %>%
        config(showLink = TRUE),
      .keep = TRUE
    ) %>%
    subplot(
      nrows = 1,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    )
}

#' Make a static density plot with ggplot2
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#' 
#' @export
#'
#' @return output A \code{ggplot} output

ggplot_densityplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression") {
  plotdata <- ggplotify(plotmatrices, experiment, colorby, value_type = 'density', annotate_samples = TRUE)
  ncats <- length(unique(plotdata$name))
  palette <- makeColorScale(ncats)
  
  p <- ggplot(data = plotdata ) +  
    geom_area(aes(x=expression, y = density, fill=name, color=name), alpha = 0.4) +
    scale_fill_manual(name = prettifyVariablename(colorby), values = makeColorScale(ncats, palette = "Set1")) +
    scale_color_manual(name = prettifyVariablename(colorby), values = makeColorScale(ncats, palette = "Set1")) +
    ylab("Density") + 
    xlab(prettifyVariablename(expressiontype)) + 
    guides(fill=guide_legend(title=prettifyVariablename(colorby))) +
    theme(legend.position="bottom")
  
  if (is.list(plotmatrices) && length(plotmatrices) > 1) {
    p <- p + facet_wrap(~ type, ncol = 1, scales = "free_y")
  }
  
  p + theme_bw(base_size = 16) + theme(
      legend.position = "bottom"
    )
}

#' Make a dynamic density plot with plotly
#' 
#' A simple function using \code{plotly} to make a sample density plot. 
#'
#' @param plotmatrices Expression/ other data matrix, or named list thereof
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressiontype Expression type for use in y axis label
#' 
#' @export
#'
#' @return output A \code{plotly} output

plotly_densityplot <- function(plotmatrices, experiment, colorby = NULL, palette = NULL, expressiontype = "expression") {

  plotdata <- ggplotify(plotmatrices, experiment, colorby, value_type = 'density', annotate_samples = TRUE)
  ncats <- length(unique(plotdata$name))
  palette <- makeColorScale(ncats)
  
  plotdata %>%
    group_by(type) %>%
    group_map(
      ~ plot_ly(
        data = .,
        x = ~ expression,
        y = ~ density,
        color = ~ name,
        type = "scatter",
        mode = "lines",
        fill = 'tozeroy',
        legendgroup = ~ name,
        colors = palette,
        showlegend = (.y == "Raw"),
        alpha = 0.2
      ) %>% layout(
        hoverlabel = list(namelength = -1),
        xaxis = list(title = prettifyVariablename(expressiontype)),
        yaxis = list(title = paste(.x$type[1])),
        legend = list(
          title = list(text = 'Sample'),
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.2
        )
      ) %>% config(showLink = TRUE),
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
#' @param ese ExploratorySummarizedExperiment
#' @param expressiontype Y axis label
#' @param whisker_distance IQR multiplier for whiskers, and beyond which to
#' show outliers (see \code{coef} in \code{\link[ggplot2]{geom_boxplot}})
#'
#' @export
#' @examples
#' data(airway, package = "airway")
#' plotly_quartiles(assays(airway)[[1]], as(airway, "ExploratorySummarizedExperiment"))
#'
plotly_quartiles <- function(matrix, ese, expressiontype = "expression", whisker_distance = 1.5) {
  matrix <- log2(matrix + 1)

  quantiles <- apply(matrix, 2, quantile, na.rm = TRUE)
  samples <- structure(colnames(matrix), names = colnames(matrix))
  iqrs <- lapply(samples, function(x) {
    quantiles["75%", x] - quantiles["25%", x]
  })

  outliers <- lapply(samples, function(x) {
    print(x)
    y <- structure(matrix[, x], names = rownames(matrix))
    ol <- y[which(y > quantiles["75%", x] + iqrs[[x]] * whisker_distance | y < quantiles["25%", x] - iqrs[[x]] * whisker_distance)]
    if (length(ol) > 0) {
      data.frame(x = x, y = ol, label = idToLabel(names(ol), ese), stringsAsFactors = FALSE)
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
