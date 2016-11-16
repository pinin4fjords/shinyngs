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
#' scatterplotInput('pca')

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
#' scatterplotOutput('pca')

scatterplotOutput <- function(id) {
    
    ns <- NS(id)
    
    list(plotlyOutput(ns("scatter"), height = "600px"))
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
#' @param input 
#' @param output 
#' @param session 
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
#' @param colorBy A reactive returning a factor definining the groups in which 
#' points should be colored.
#' @param getPalette An optional palette of colors, one for each level of 
#' colorBy.
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

scatterplot <- function(input, output, session, getDatamatrix, getThreedee = NULL, getXAxis = NULL, getYAxis = NULL, getZAxis = NULL, getShowLabels = NULL, 
    getPointSize = NULL, getPalette = NULL, colorBy = NULL, getTitle = reactive({
        ""
    }), getLabels = reactive({
        rownames(getDatamatrix())
    }), allow_3d = TRUE, x = NA, y = NA, z = NA, getLines = NULL) {
    
    # If inputs are not provided, render controls to provide them
    
    ns <- session$ns
    
    # If no colors are provided, make our own if necessary. This will cause the 'getPalette' reactive to be passed back from scatterplotcontrols.
    
    getNumberColors <- reactive({
        make_colors <- is.null(getPalette) && !is.null(colorBy)
        if (make_colors) {
            cb <- colorBy()
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
        unpack.list(callModule(scatterplotcontrols, "scatter", getDatamatrix, x = x, y = y, z = z, makeColors = getNumberColors))
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
            label_offset_y <- (max(getDatamatrix()[[getYAxis()]]) - min(getDatamatrix()[[getYAxis()]]))/40
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
        if (is.null(colorBy)) {
            FALSE
        } else {
            TRUE
        }
    })
    
    # Unlabelled points will be plotted without hovers etc.
    
    unlabelled <- reactive(is.na(getLabels()))
    
    addUnlabelledPoints <- function(p) {
        if (any(unlabelled())) {
            withProgress(message = "Adding unlabelled points", value = 0, {
                
                plotargs <- list(p, x = xdata()[unlabelled()], y = ydata()[unlabelled()], z = zdata()[unlabelled()], mode = "markers", hoverinfo = "none", 
                  type = plotType(), showlegend = showLegend(), name = "unselected rows", marker = list(size = getPointSize() - 2, color = "gray"))
                
                p <- do.call(plotly::add_trace, plotargs)
                
            })
        }
        p
    }
    
    # makeColorScale <- reactive({ ncolors <- nlevels(factor(colorBy())) if (ncolors > brewer.pal.info['Set1', 'maxcolors']){ cols <-
    # colorRampPalette(brewer.pal(brewer.pal.info['Set1', 'maxcolors'], 'Set1'))(ncolors) }else{ cols <- RColorBrewer::brewer.pal(ncolors, 'Set1') } rev(cols)
    # })
    
    # Labelled points plotted with hovers, colors as specified in groupings
    
    addLabelledPoints <- function(p) {
        if (any(!unlabelled())) {
            withProgress(message = "Adding labelled points", value = 0, {
                plotargs <- list(p, x = xdata()[!unlabelled()], y = ydata()[!unlabelled()], z = zdata()[!unlabelled()], mode = "markers", hoverinfo = "text", 
                  text = getLabels()[!unlabelled()], type = plotType(), showlegend = showLegend(), marker = list(size = getPointSize()))
                
                if (!is.null(colorBy)) {
                  plotargs$color <- colorBy()[!unlabelled()]
                }
                
                p <- do.call(plotly::add_trace, plotargs)
            })
        }
        p
    }
    
    # Show actual text labels if specified
    
    addTextLabels <- function(p) {
        
        if (getShowLabels()) {
            labelargs <- list(p, x = xdata()[!unlabelled()], y = yLabData()[!unlabelled()], z = zdata()[!unlabelled()], mode = "text", text = getLabels()[!unlabelled()], 
                type = plotType(), hoverinfo = "none", showlegend = FALSE)
            
            if (!is.null(colorBy)) {
                labelargs$color <- colorBy()[!unlabelled()]
                labelargs$colors <- getPalette()
            }
            
            p <- do.call(add_trace, labelargs)
        }
        p
    }
    
    # Do the layout
    
    adjustLayout <- function(p, title = "") {
        
        withProgress(message = "Adjusting axis display", value = 0, {
            
            axis_layouts <- list(xaxis = list(title = colnames(getDatamatrix())[getXAxis()]), yaxis = list(title = colnames(getDatamatrix())[getYAxis()]), 
                legend = list(y = 0.8))
            
            layoutArgs <- reactive({
                la <- c(list(p, hovermode = "closest", title = title), axis_layouts)
                
                if (getThreedee()) {
                  axis_layouts$zaxis <- list(title = colnames(getDatamatrix())[getZAxis()])
                  la$scene <- axis_layouts
                }
                la
            })
            
            p <- do.call(plotly::layout, layoutArgs())
            
        })
        p
    }
    
    # Draw any speicfied lines on the plot
    
    drawLines <- function(p) {
        
        if (!is.null(getLines)) {
            withProgress(message = "Drawing lines", value = 0, {
                lines <- getLines()
                lines <- group_by(lines, name)
                
                p <- add_lines(p, data = lines, x = ~x, y = ~y, linetype = ~name, line = list(color = "black"))
            })
            
        }
        p
    }
    
    # Chain the various steps together.
    
    output$scatter <- renderPlotly({
        withProgress(message = "Drawing scatter plot", value = 0, {
            
            plotargs <- list(type = plotType(), mode = "markers")
            
            if (!is.null(colorBy)) {
                
                # If a palette was supplied, or if we made our own...
                
                if (is.null(getPalette)) {
                  plotargs$colors <- getScatterPalette()
                } else {
                  plotargs$colors <- getPalette()
                }
            }
            
            do.call(plot_ly, plotargs) %>% addUnlabelledPoints() %>% addLabelledPoints() %>% drawLines() %>% addTextLabels() %>% adjustLayout(title = getTitle()) %>% 
                config(showLink = TRUE)
        })
    })
} 
