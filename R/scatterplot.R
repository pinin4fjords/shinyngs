#' Input function for the scatterplot module
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
#' @export
#'
#' @examples
#' scatterplotInput('pca')

scatterplotInput <- function(id) {
    
    ns <- NS(id)
    uiOutput(ns("controls"))
}

#' Output function for the scatterplot module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character()
#' @export
#'
#' @examples
#' scatterplotOutput('pca')

scatterplotOutput <- function(id) {
    
    ns <- NS(id)
    
    list(plotlyOutput(ns("scatter"), height = "600px"))
}

#' Server function for the scatterplot module
#'
#' Controls for this module are provided by the \code{scatterplotcontrols}
#' module, which is automatically called if reactives are not supplied to the 
#' server function. This setup allows the same set of controls to power 
#' multiple scatter plots.
#' 
#' Plots are powered by plotly
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
#' @param title A plot title
#' @param labels An optional list of labels to use instead of row names from
#' \code{getDatamatrix()}.
#' @param colorby A reactive returning a factor definining the groups in which 
#' points should be colored.
#' @param allow_3d For case where controls are generated, passed to  
#' \code{scatterplotcontrols}.
#' @param x For case where controls are generated, passed to  
#' \code{scatterplotcontrols}.
#' @param y For case where controls are generated, passed to  
#' \code{scatterplotcontrols}.
#' @param z For case where controls are generated, passed to  
#' \code{scatterplotcontrols}.
#' @param getLines Reactive returning a data frame defining lines to be drawn.
#' Three columns required: name, x and y, with two rows for every value of
#' name. These two rows represent the start and end of a line.
#'
#' @export
#'
#' @examples
#' callModule(scatterplot, 'pca', getDatamatrix = pcaMatrix, getThreedee = getThreedee, getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis, getShowLabels = getShowLabels, getPointSize = getPointSize, title = 'PCA plot', colorby = na.replace(selectColData()[[colorBy()]], 'N/A'))

scatterplot <- function(input, output, session, getDatamatrix, getThreedee = NULL, getXAxis = NULL, getYAxis = NULL, getZAxis = NULL, getShowLabels = NULL, getPointSize = NULL, 
    title = "", getLabels = reactive({
        rownames(getDatamatrix())
    }), colorby = NULL, size = NULL, allow_3d = TRUE, x = NA, y = NA, z = NA, getLines = NULL) {
    
    # If inputs are not provided, render controls to provide them
    
    if (is.null(getThreedee)) {
        output$controls <- renderUI({
            ns <- session$ns
            scatterplotcontrolsInput(ns("scatter"), allow_3d = allow_3d)
        })
        unpack.list(callModule(scatterplotcontrols, "scatter", getDatamatrix, x = x, y = y, z = z))
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
        if (is.null(colorby)) {
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
                
                plotargs <- list(p, x = xdata()[unlabelled()], y = ydata()[unlabelled()], z = zdata()[unlabelled()], mode = "markers", hoverinfo = "none", type = plotType(), showlegend = showLegend(), 
                  name = "unselected rows", marker = list(size = getPointSize() - 2, color = "gray"))
                
                p <- do.call(plotly::add_trace, plotargs)
                
            })
        }
        p
    }
    
    makeColorScale <- reactive({
        ncolors <- nlevels(factor(colorby()))
        
        rev(RColorBrewer::brewer.pal(ncolors, "Set1"))
    })
    
    # Labelled points plotted with hovers, colors as specified in groupings
    
    addLabelledPoints <- function(p) {
        if (any(!unlabelled())) {
            withProgress(message = "Adding labelled points", value = 0, {
                plotargs <- list(p, x = xdata()[!unlabelled()], y = ydata()[!unlabelled()], z = zdata()[!unlabelled()], mode = "markers", hoverinfo = "text", text = getLabels()[!unlabelled()], 
                  type = plotType(), showlegend = showLegend(), marker = list(size = getPointSize()))
                
                if (!is.null(colorby)) {
                  plotargs$color <- colorby()[!unlabelled()]
                  plotargs$colors = makeColorScale()
                }
                
                p <- do.call(plotly::add_trace, plotargs)
            })
        }
        p
    }
    
    # Show actual text labels if specified
    
    addTextLabels <- function(p) {
        # Show specified labels
        
        if (getShowLabels()) {
            labelargs <- list(p, x = xdata()[!unlabelled()], y = yLabData()[!unlabelled()], z = zdata()[!unlabelled()], mode = "text", text = getLabels()[!unlabelled()], type = plotType(), 
                hoverinfo = "none", showlegend = FALSE)
            
            if (!is.null(colorby)) {
                labelargs$color <- colorby()[!unlabelled()]
                labelargs$colors = makeColorScale()
            }
            
            p <- do.call(add_trace, labelargs)
        }
        p
    }
    
    # Do the layout
    
    adjustLayout <- function(p) {
        
        withProgress(message = "Adjusting axis display", value = 0, {
            
            axis_layouts <- list(xaxis = list(title = colnames(getDatamatrix())[getXAxis()]), yaxis = list(title = colnames(getDatamatrix())[getYAxis()]), zaxis = list(title = colnames(getDatamatrix())[getZAxis()]), 
                legend = list(y = 0.8))
            
            layoutArgs <- reactive({
                la <- c(list(p, hovermode = "closest", title = title), axis_layouts)
                
                if (getThreedee()) {
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
                
                p <- plotly::add_trace(lines, x = lines$x, y = lines$y, group = lines$name, mode = "lines", line = list(color = "black", dash = 6, width = 1), showlegend = FALSE, 
                  name = i)
            })
            
        }
        p
    }
    
    # Chain the various steps together
    
    output$scatter <- renderPlotly({
        
        plot_ly(type = plotType()) %>% addUnlabelledPoints() %>% addLabelledPoints() %>% adjustLayout() %>% addTextLabels() %>% drawLines()
        
    })
} 
