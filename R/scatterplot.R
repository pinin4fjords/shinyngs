#' Input function for the scatterplot module
#'
#' This module uses \href{https://plot.ly/}{Plotly} to create scatter plots
#' (see \code{\link[plotly]{plot_ly}}), of both 2D and 3D varieties.
#'
#' Controls for this module are provided by the \code{scatterplotcontrols}
#' module, which is automatically called if reactives are not supplied to the
#' server function. This setup allows the same set of controls to power
#' multiple scatter plots.
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @examples
#' scatterplotInput("pca")
#'
scatterplotInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("controls"))
}

#' Output function for the scatterplot module
#'
#' This module uses \href{https://plot.ly/}{Plotly} to create scatter plots
#' (see \code{\link[plotly]{plot_ly}}), of both 2D and 3D varieties.
#'
#' Controls for this module are provided by the \code{scatterplotcontrols}
#' module, which is automatically called if reactives are not supplied to the
#' server function. This setup allows the same set of controls to power
#' multiple scatter plots.
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @examples
#' scatterplotOutput("pca")
#'
scatterplotOutput <- function(id) {
  ns <- NS(id)

  list(shinycssloaders::withSpinner(plotlyOutput(ns("scatter"), height = "600px"), color = shinyngsSpinnerColor()))
}

#' Server function for the scatterplot module
#'
#' This module uses \href{https://plot.ly/}{Plotly} to create scatter plots
#' (see \code{\link[plotly]{plot_ly}}), of both 2D and 3D varieties.
#'
#' Controls for this module are provided by the \code{scatterplotcontrols}
#' module, which is automatically called if reactives are not supplied to the
#' server function. This setup allows the same set of controls to power
#' multiple scatter plots.
#'
#' @param id Module namespace
#' @param getDatamatrix Reactive supplying a matrix. If using external controls
#' this should match the one supplied to \code{scatterplotcontrols}
#' @param getThreedee A reactive defining whether to plot in 3D. If set to NULL
#' (default), the \code{scatterplotcontrols} module will be called to create
#' a set of inputs to supply this value and the axes etc.
#' @param getXAxis NULL, or if \code{getThreedee} is a reactive, a reactive
#' supplying an integer specifying which column of the matrix supplied by
#' \code{getDatamatrix} should be used for this axis.
#' @param getYAxis NULL, or if \code{getThreedee} is a reactive, a reactive
#' supplying an integer specifying which column of the matrix supplied by
#' \code{getDatamatrix} should be used for this axis.
#' @param getZAxis NULL, or if \code{getThreedee} is a reactive, a reactive
#' supplying an integer specifying which column of the matrix supplied by
#' \code{getDatamatrix} should be used for this axis.
#' @param getShowLabels NULL, or if \code{getThreedee} is a reactive, a
#' reactive supplying a logical defining whether labels should be shown on
#' points.
#' @param getPointSize NULL, or if \code{getThreedee} is a reactive, a
#' reactive supplying an integer point size to pass to plotly.
#' @param getTitle A reactive expression supplying a title.
#' @param getLabels A reactive supplying a list of labels to use instead of row
#' names from \code{getDatamatrix()}
#' @param getColorby A reactive returning a factor definining the groups in
#' which points should be colored.
#' @param getPalette An optional palette of colors, one for each level of
#' getColorby.
#
#' @param allow_3d Passed to \code{\link{scatterplotcontrolsInput}} to dermine
#' if the user will be allowed to create 3D plots.
#' @param x Passed to \code{\link{scatterplotcontrolsInput}} to determine how
#' it produces an input field for selecting the x axis. A value supplied for
#' this parameter will cause a hidden field to be generated instead of a
#' select, useful for scatter plots that don't need the user to select axes
#' (default: NA).
#' @param y Passed to \code{\link{scatterplotcontrolsInput}} to determine how
#' it produces an input field for selecting the y axis. A value supplied for
#' this parameter will cause a hidden field to be generated instead of a
#' select, useful for scatter plots that don't need the user to select axes
#' (default: NA).
#' @param z Passed to \code{\link{scatterplotcontrolsInput}} to determine how
#' it produces an input field for selecting the z axis. A value supplied for
#' this parameter will cause a hidden field to be generated instead of a
#' select, useful for scatter plots that don't need the user to select axes
#' (default: NA).
#' @param getLines Reactive returning a data frame defining lines to be drawn.
#' Three columns required: name, x and y, with two rows for every value of
#' name. These two rows represent the start and end of a line.

scatterplot <- function(id, getDatamatrix, getThreedee = NULL, getXAxis = NULL, getYAxis = NULL, getZAxis = NULL, getShowLabels = NULL, getPointSize = NULL, getPalette = NULL, getColorby = NULL, getTitle = reactive({
                          ""
                        }), getLabels = reactive({
                          rownames(getDatamatrix())
                        }), allow_3d = TRUE, x = NA, y = NA, z = NA, getLines = reactive({
                          NULL
                        })) {
  moduleServer(id, function(input, output, session) {
    # If inputs are not provided, render controls to provide them

    ns <- session$ns

    # If no colors are provided, make our own if necessary. This will cause the 'getPalette' reactive to be passed back from scatterplotcontrols.

    getNumberColors <- reactive({
      make_colors <- is.null(getPalette) && !is.null(getColorby)
      if (make_colors) {
        cb <- getColorby()
        nlevels(cb)
      } else {
        NULL
      }
    })

    # getThreedee used to determine whether the controls were provided.

    if (is.null(getThreedee)) {
      output$controls <- renderUI({
        make_colors <- !is.null(getNumberColors())
        controls <- list(scatterplotcontrolsInput(ns("scatter"), allow_3d = allow_3d, make_colors = make_colors))
      })

      # Provide the reactives from the scatterplotcontrols module in place of the (unsupplied) arguments of the same name

      scatterplotcontrols_reactives <- scatterplotcontrols("scatter", getDatamatrix, x = x, y = y, z = z, makeColors = getNumberColors)
      getThreedee <- scatterplotcontrols_reactives$getThreedee
      getXAxis <- scatterplotcontrols_reactives$getXAxis
      getYAxis <- scatterplotcontrols_reactives$getYAxis
      getZAxis <- scatterplotcontrols_reactives$getZAxis
      getShowLabels <- scatterplotcontrols_reactives$getShowLabels
      getPointSize <- scatterplotcontrols_reactives$getPointSize
      getScatterPalette <- scatterplotcontrols_reactives$getScatterPalette
    }

    # Axis data accessors

    xdata <- reactive({
      getDatamatrix()[, getXAxis()]
    })

    ydata <- reactive({
      getDatamatrix()[, getYAxis()]
    })

    zdata <- reactive({
      if (is.null(getZAxis())) {
        NULL
      } else {
        getDatamatrix()[, getZAxis()]
      }
    })

    # Slight offset for labels on the y axis

    yLabData <- reactive({
      if (!getThreedee()) {
        label_offset_y <- (max(getDatamatrix()[[getYAxis()]]) - min(getDatamatrix()[[getYAxis()]])) / 40
        ydata() + label_offset_y
      } else {
        ydata()
      }
    })

    # Choose the right plot type

    plotType <- reactive({
      if (getThreedee()) {
        "scatter3d"
      } else {
        "scatter"
      }
    })

    # Only show a legend if we're coloring points

    showLegend <- reactive({
      if (is.null(getColorby)) {
        FALSE
      } else {
        TRUE
      }
    })

    # Chain the various steps together.

    output$scatter <- renderPlotly({
      withProgress(message = "Drawing scatter plot", value = 0, {
        if (!is.null(getColorby)) {
          cb <- getColorby()

          # If a palette was supplied, or if we made our own...

          if (is.null(getPalette)) {
            palette <- getScatterPalette()
          } else {
            palette <- getPalette()
          }
        } else {
          cb <- NULL
          palette <- NULL
        }

        plotly_scatterplot(
          x = xdata(), y = ydata(), z = zdata(), colorby = cb, plot_type = plotType(), title = getTitle(),
          xlab = colnames(getDatamatrix())[getXAxis()], ylab = colnames(getDatamatrix())[getYAxis()],
          zlab = colnames(getDatamatrix())[getZAxis()], palette = palette, labels = getLabels(),
          show_labels = getShowLabels(), lines = getLines(), showlegend = showLegend(),
          point_size = getPointSize()
        ) %>%
          shinyngsPlotlyConfig("scatterplot", format = session$userData$plotFormat())
      })
    })
  })
}

#' Add points to a plotly object
#'
#' @param p Previously generated plotly object
#' @param x Vector of numeric x values
#' @param y Vector of numeric y values
#' @param z Optional vector of numeric z values
#' @param colorby String vector or factor specifying value groups
#' @param name Name for the series
#' @param label Boolean- should points be colored and labelled?
#' @param plot_type Plot type: 'scatter' or 'scatter3d'
#' @param point_size Main point size
#' @param labels Vector of labels to apply (if 'label' is TRUE)
#' @param showlegend Boolean: show this set of points in the legend?
#'
#' @return output Plotly plot object

addPoints <- function(p, x, y, z = NULL, colorby = NULL, name = NULL, label = FALSE, plot_type = "scatter", point_size = 5, labels = NULL, showlegend = FALSE) {
  plotargs <- list(
    p,
    x = x,
    y = y,
    z = z,
    mode = "markers",
    type = plot_type,
    showlegend = showlegend,
    name = name
  )

  if (label) {
    plotargs$hoverinfo <- "text"
    plotargs$marker <- list(size = point_size)
    plotargs$text <- labels
  } else {
    plotargs$hoverinfo <- "none"
    plotargs$marker <- list(size = point_size - 2, color = "gray")
  }

  if (!is.null(colorby)) {
    plotargs$color <- colorby
  }

  do.call(plotly::add_trace, plotargs)
}

#' Add permanent text labels to points in a plotly graph
#'
#' @param p Previously generated plotly object
#' @param x Vector of numeric x values
#' @param y Vector of numeric y values
#' @param z Optional vector of numeric z values
#' @param colorby String vector or factor specifying value groups
#' @param labels Vector of labels to apply
#' @param show_labels If false, simpy pass through input plot object
#' @param plot_type Plot type: 'scatter' or 'scatter3d'
#'
#' @return output Plotly object

addTextLabels <- function(p, x, y, z, colorby = NULL, labels, plot_type, show_labels = TRUE) {
  if (show_labels) {
    labelargs <- list(
      p,
      x = x,
      y = y,
      z = z,
      mode = "text",
      text = labels,
      type = plot_type,
      hoverinfo = "none",
      showlegend = FALSE
    )

    if (!is.null(colorby)) {
      labelargs$color <- colorby
    }

    p <- do.call(add_trace, labelargs)
  }
  p
}

#' Overlay lines on a plotly-generated plot
#'
#' @param p Previously generated plotly object
#' @param x X coordinates of points, used to determine x range
#' @param y Y coordinates of points, used to determine y range
#' @param lines 3 column data-frame (name, x, y) with two rows, one for the
#'   start and end of each named line
#' @param hline_thresholds Alternatively or in addition to 'lines', just specify
#'   a named list of y values at which to place hlines
#' @param vline_thresholds Alternatively or in addition to 'lines', just specify
#'   a named list of x values at which to place vlines
#' @param plot_type Plot type: 'scatter' or 'scatter3d'. The axis range fix
#'   that makes threshold lines reach the plot edges only applies to 2D plots.
#' @param xrange Optional fixed c(min, max) x axis range. When NULL (the
#'   default) the range is derived from the data with 5% padding, as before.
#' @param yrange Optional fixed c(min, max) y axis range. When NULL (the
#'   default) the range is derived from the data with 5% padding, as before.
#'
#' @return output Plotly object

drawLines <- function(p, x, y, lines = NULL, hline_thresholds = list(), vline_thresholds = list(), plot_type = "scatter",
                       xrange = NULL, yrange = NULL) {
  line_coords <- list()
  if (!is.null(lines)) {
    line_coords[["specified"]] <- lines
  }

  if (length(hline_thresholds) > 0) {
    line_coords$h <- do.call(rbind, lapply(names(hline_thresholds), function(hl) {
      data.frame(x = c(min(x[is.finite(x)]), max(x[is.finite(x)])), y = c(rep(hline_thresholds[[hl]], 2)), name = hl)
    }))
  }
  if (length(vline_thresholds) > 0) {
    line_coords$v <- do.call(rbind, lapply(names(vline_thresholds), function(vl) {
      data.frame(x = rep(vline_thresholds[[vl]], 2), y = c(min(y[is.finite(y)]), max(y[is.finite(y)])), name = vl)
    }))
  }

  if (length(line_coords) > 0) {
    lines <- do.call(rbind, line_coords)

    # Horizontal/vertical threshold lines are conventionally meant to span
    # the whole plot, but their endpoints are only ever known in terms of the
    # point data range. Plotly's own axis autorange then pads beyond that
    # range, leaving a visible gap between the line ends and the plot edges.
    # Extending the lines to a slightly padded data range, and pinning the
    # axis range to match, closes that gap so the lines reach the edges.
    # A caller-supplied xrange/yrange is used as-is instead, e.g. so a
    # volcano plot's threshold lines can span a symmetric fold-change range
    # rather than the plotted points' raw (and possibly asymmetric) extent.
    if (plot_type != "scatter3d") {
      if (is.null(xrange)) {
        xrange <- range(x[is.finite(x)])
        xrange <- xrange + c(-1, 1) * diff(xrange) * 0.05
      }
      if (is.null(yrange)) {
        yrange <- range(y[is.finite(y)])
        yrange <- yrange + c(-1, 1) * diff(yrange) * 0.05
      }

      lines <- do.call(rbind, lapply(split(lines, lines$name), function(segment) {
        if (length(unique(segment$y)) == 1) {
          segment$x <- xrange
        } else if (length(unique(segment$x)) == 1) {
          segment$y <- yrange
        }
        segment
      }))
    }

    lines <- group_by(lines, name)

    p <- add_lines(p, data = lines, x = ~x, y = ~y, linetype = ~name, line = list(color = "black"))

    if (plot_type != "scatter3d") {
      p <- plotly::layout(p, xaxis = list(range = xrange), yaxis = list(range = yrange))
    }
  }
  p
}

#' Apply layout adjustments to plotly object
#'
#' @param p Previously generated plotly object
#' @param title Plot title
#' @param legend_title Legend title
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param zlab Z axis label
#' @param plot_type Plot type: 'scatter' or 'scatter3d'
#'
#' @return output Plotly object

adjustLayout <- function(p, title = "", legend_title = "", xlab = "x", ylab = "y", zlab = "z", plot_type = "scatter") {
  axis_layouts <- list(
    xaxis = list(title = xlab), yaxis = list(title = ylab),
    legend = list(title = list(text = legend_title), y = 0.8)
  )

  layout_args <- c(list(p, hovermode = "closest", title = title), axis_layouts)

  if (plot_type == "scatter3d") {
    axis_layouts$zaxis <- list(title = zlab)
    layout_args$scene <- axis_layouts
  }

  p <- do.call(plotly::layout, layout_args)
  p
}

#' Colour points by a chosen variable, with a plotly dropdown to switch it
#'
#' Builds one set of per-level marker traces per entry of \code{colorby_menu},
#' all but the first hidden initially, plus a plotly \code{updatemenus}
#' dropdown that toggles which set is visible. This is a standalone
#' alternative to the labelled/unselected trace pair built by
#' \code{\link{addPoints}}, used only when a caller wants readers to be able
#' to recolour the plot themselves (e.g. a static report); it does not
#' support a labelled/unselected point split.
#'
#' @param x X coordinates
#' @param y Y coordinates
#' @param z Optional Z coordinates
#' @param colorby_menu Named list of string vectors/factors, one per dropdown
#'   option, each the same length as x/y(/z)
#' @param labels Optional hover labels, constant across dropdown options
#' @param plot_type Plot type: 'scatter' or 'scatter3d'
#' @param point_size Main point size
#' @param title Base plot title; each dropdown option appends "coloured by X"
#' @param palette_name Valid R color palette name, applied per option
#'
#' @return output Plotly plot object

addColorbyMenu <- function(x, y, z = NULL, colorby_menu, labels = NULL, plot_type = "scatter", point_size = 5,
                           title = "", palette_name = COLORBLIND_PALETTE_NAME) {
  option_names <- names(colorby_menu)

  fig <- plot_ly()
  trace_indices <- list()
  trace_counter <- 0L

  for (option_name in option_names) {
    option_colorby <- colorby_menu[[option_name]]
    if (!is.factor(option_colorby)) {
      option_colorby <- factor(option_colorby, levels = unique(option_colorby))
    }
    option_palette <- makeColorScale(length(levels(option_colorby)), palette = palette_name)
    start_idx <- trace_counter + 1L

    for (j in seq_along(levels(option_colorby))) {
      idx <- which(option_colorby == levels(option_colorby)[j])
      trace_counter <- trace_counter + 1L

      trace_args <- list(
        fig,
        x = x[idx],
        y = y[idx],
        name = levels(option_colorby)[j],
        legendgroup = option_name,
        showlegend = (option_name == option_names[1]),
        visible = (option_name == option_names[1]),
        marker = list(size = point_size, color = option_palette[j])
      )

      if (plot_type == "scatter3d") {
        trace_args$z <- z[idx]
        trace_args$type <- "scatter3d"
      }

      if (!is.null(labels)) {
        trace_args$text <- labels[idx]
        trace_args$hoverinfo <- "text+x+y"
      }

      fig <- do.call(plotly::add_markers, trace_args)
    }
    trace_indices[[option_name]] <- start_idx:trace_counter
  }

  total_traces <- trace_counter

  # The plotly modebar (camera/zoom/pan icons) is docked top-right, so the
  # dropdown is placed top-left to avoid sitting on top of it.
  updatemenus <- list(list(
    type = "dropdown", active = 0,
    x = 0, xanchor = "left", y = 1.15, yanchor = "top",
    buttons = lapply(option_names, function(option_name) {
      visible <- rep(FALSE, total_traces)
      visible[trace_indices[[option_name]]] <- TRUE
      option_title <- if (nzchar(title)) paste0(title, " (coloured by ", option_name, ")") else paste("Coloured by", option_name)

      list(
        label = option_name,
        method = "update",
        args = list(
          list(visible = visible, showlegend = visible),
          list(title = option_title, legend = list(title = list(text = option_name)))
        )
      )
    })
  ))

  # The initial title/legend are left to the caller (adjustLayout), which
  # runs after this and must agree with the first dropdown option so the
  # page load state matches what re-selecting that option would produce.
  plotly::layout(fig, updatemenus = updatemenus, margin = list(t = 80))
}

#' Make scatterplots with \code{plot_ly()}
#'
#' @param x X coordinates
#' @param y Y coordinates
#' @param z Optional Z coordinates
#' @param colorby String vector or factor specifying value groups
#' @param plot_type Plot type: 'scatter' or 'scatter3d'
#' @param title Plot title
#' @param legend_title Legend title
#' @param xlab X label
#' @param ylab Y label
#' @param zlab Z label
#' @param palette Color palette correct for the number of groups in 'colorby'
#' @param point_size Main point size
#' @param labels Point labels
#' @param show_labels Permanently show labels for labelled points (default is just on hoverover).
#'   Ignored when \code{colorby_menu} is supplied.
#' @param lines 3 column data-frame (name, x, y) with two rows, one for the
#'   start and end of each named line
#' @param hline_thresholds Named list of horizontal lines with y coordinates
#' @param vline_thresholds Named list of vertical lines x coordinates
#' @param xrange Optional fixed c(min, max) x axis range, e.g. to keep a
#'   volcano plot symmetric around zero. Only applied when lines/thresholds
#'   are drawn; otherwise plotly's own autorange is used. Defaults to NULL
#'   (derive from the data, as before).
#' @param yrange Optional fixed c(min, max) y axis range. Same caveats as
#'   \code{xrange}.
#' @param showlegend Boolean: show a legend?
#' @param palette_name Valid R color palette name
#' @param colorby_menu Opt-in, named list of alternative colour vectors (one
#'   per entry, same length as x/y(/z)). When supplied, the plot is coloured
#'   using \code{\link{addColorbyMenu}} instead of \code{colorby}, and a
#'   plotly dropdown is added letting the reader switch which variable
#'   colours the points; \code{colorby} is not shown itself unless also
#'   included as an entry in \code{colorby_menu}. This is intended for
#'   standalone/static-HTML report use only: the Shiny scatterplot modules
#'   already offer their own colour-by \code{selectInput}, and must NOT set
#'   this argument, since an always-on in-widget dropdown alongside that
#'   control would give readers two competing ways to change the colouring.
#'   Default is NULL (off).
#'
#' @return output Plotly plot object
#' @export

plotly_scatterplot <- function(x, y, z = NULL, colorby = NULL, plot_type = "scatter", title = "", legend_title = "",
                               xlab = "x", ylab = "y", zlab = "z", palette = NULL, point_size = 5, labels = NULL,
                               show_labels = FALSE, lines = NULL, hline_thresholds = NULL, vline_thresholds = NULL,
                               xrange = NULL, yrange = NULL, showlegend = TRUE, palette_name = COLORBLIND_PALETTE_NAME,
                               colorby_menu = NULL) {
  if (!is.null(colorby_menu)) {
    p <- addColorbyMenu(
      x = x,
      y = y,
      z = z,
      colorby_menu = colorby_menu,
      labels = labels,
      plot_type = plot_type,
      point_size = point_size,
      title = title,
      palette_name = palette_name
    )

    # The page loads on the first dropdown option, so the initial
    # title/legend below must match what selecting it would produce.
    first_option <- names(colorby_menu)[1]
    title <- if (nzchar(title)) paste0(title, " (coloured by ", first_option, ")") else paste("Coloured by", first_option)
    legend_title <- first_option
  } else {
    # We'll only label and color points with non-NA labels

    if (is.null(labels)) {
      labelled <- rep(FALSE, length(x))
    } else {
      labelled <- !is.na(labels)
    }

    if ((!is.null(colorby)) && !is.factor(colorby)) {
      colorby <- factor(colorby)
    }

    if (is.null(palette)) {
      if (any(labelled) && !is.null(colorby)) {
        palette <- makeColorScale(length(unique(colorby[labelled])), palette = palette_name)
      } else {
        palette <- makeColorScale(1)
      }
    }

    plotargs <- list(
      type = plot_type,
      mode = "markers",
      colors = palette
    )

    # Nudge to be used with text labels
    nudge_y <- (max(y) - min(y)) / 50

    p <- do.call(plot_ly, plotargs) %>%
      addPoints(
        x = x[!labelled],
        y = y[!labelled],
        z = z[!labelled],
        name = "unselected rows",
        label = FALSE,
        plot_type = plot_type,
        point_size = point_size,
        colorby = NULL,
        showlegend = showlegend
      ) %>%
      addPoints(
        x = x[labelled],
        y = y[labelled],
        z = z[labelled],
        label = TRUE,
        plot_type = plot_type,
        point_size = point_size,
        labels = labels[labelled],
        colorby = colorby[labelled],
        showlegend = showlegend
      ) %>%
      addTextLabels(
        x = x[labelled],
        y = y[labelled] + nudge_y,
        z = z[labelled],
        plot_type = plot_type,
        labels = labels[labelled],
        colorby = colorby[labelled],
        show_labels = show_labels
      )
  }

  p %>%
    drawLines(
      x = x,
      y = y,
      lines = lines,
      hline_thresholds = hline_thresholds,
      vline_thresholds = vline_thresholds,
      plot_type = plot_type,
      xrange = xrange,
      yrange = yrange
    ) %>%
    adjustLayout(
      title = title,
      legend_title = legend_title,
      xlab = xlab,
      ylab = ylab,
      zlab = zlab,
      plot_type = plot_type
    )
}

#' Make scatterplots with \code{ggplot()} or \code{scatterplot3d}
#'
#' These are not used in the shinyngs UI, but are provided here to be fairly
#' consistent with the plotly-driven display, and provide a static alternative
#' for external users.
#'
#' @param x X coordinates
#' @param y Y coordinates
#' @param z Optional Z coordinates
#' @param colorby String vector or factor specifying value groups
#' @param plot_type Plot type: 'scatter' (ggplot) or 'scatter3d' (scatterplot3d)
#' @param title Plot title
#' @param legend_title Legend title
#' @param xlab X label
#' @param ylab Y label
#' @param zlab Z label
#' @param palette Color palette correct for the number of groups in 'colorby'
#' @param point_size Main point size
#' @param labels Point labels
#' @param show_labels Permanently show labels for labelled points
#' @param hline_thresholds Named list of horizontal lines with y coordinates
#' @param vline_thresholds Named list of vertical lines x coordinates
#' @param showlegend Boolean: show a legend?
#' @param palette_name Valid R color palette name
#'
#' @import scatterplot3d
#' @export
#'
#' @return output Ouput object from ggplot or scatterplot3d.

static_scatterplot <- function(x, y, z = NULL, colorby = NULL, plot_type = "scatter", title = "", legend_title = NULL,
                               xlab = "x", ylab = "y", zlab = "z", palette = NULL, point_size = 1, labels = colorby,
                               show_labels = FALSE, hline_thresholds = NULL, vline_thresholds = NULL, showlegend = TRUE,
                               palette_name = COLORBLIND_PALETTE_NAME) {
  labelled <- !is.na(labels)

  if ((!is.null(colorby)) && !is.factor(colorby)) {
    colorby <- factor(colorby)
  }

  if (is.null(palette)) {
    if (!is.null(colorby)) {
      palette <- makeColorScale(length(unique(colorby)), palette = palette_name)
    } else {
      palette <- makeColorScale(1)
    }
  }

  if (plot_type == "scatter") {
    plotdata <- data.frame(
      x = x,
      y = y,
      colorby = colorby
    )

    if (!is.null(labels)) {
      plotdata$label <- labels
    }

    p <- ggplot(
      plotdata,
      aes(
        x = x,
        y = y,
        color = colorby,
        label = label
      )
    ) +
      geom_point(size = point_size) +
      scale_color_manual(name = legend_title, values = palette)


    if (show_labels) {
      p <- p + geom_text(
        data = subset(plotdata, !is.na(labels)),
        hjust = "inward",
        show.legend = FALSE,
        nudge_y = (max(y) - min(y)) / 50
      )
    }

    if (!is.null(hline_thresholds)) {
      p <- p +
        geom_hline(
          data = data.frame(
            type = names(hline_thresholds),
            yintercept = unlist(hline_thresholds)
          ),
          aes(
            yintercept = yintercept,
            linetype = type
          )
        )
    }

    if (!is.null(vline_thresholds)) {
      p <- p +
        geom_vline(
          data = data.frame(
            type = names(vline_thresholds),
            xintercept = unlist(vline_thresholds)
          ),
          aes(
            xintercept = xintercept,
            linetype = type
          )
        )
    }

    if ((!is.null(vline_thresholds)) || !is.null(hline_thresholds)) {
      p <- p + guides(linetype = guide_legend(title = "Lines"))
    }

    p + theme_bw(base_size = 16) +
      xlab(xlab) +
      ylab(ylab)
  } else if (plot_type == "scatter3d") {
    colorby_idx <- as.numeric(colorby)

    s3d <- scatterplot3d(
      x = x,
      y = y,
      z = z,
      xlab = xlab,
      ylab = ylab,
      zlab = zlab,
      pch = 16,
      color = unlist(lapply(colorby_idx, function(c) palette[c]))
    )
    if (showlegend) {
      legend("topright", s3d$xyz.convert(18, 0, 12),
        pch = 16, yjust = 0, col = palette[unique(colorby_idx)],
        legend = c(unique(colorby)), cex = 1.1, title = legend_title
      )
    }
    if (show_labels) {
      s3d.coords <- s3d$xyz.convert(x, y, z)
      text(s3d.coords$x,
        s3d.coords$y,
        labels = labels,
        pos = 4
      )
    }
  }
}
