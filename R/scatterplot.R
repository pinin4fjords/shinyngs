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
#' @param colorby A factor definining the groups in which points should be 
#' colored.
#'
#' @export
#'
#' @examples
#' callModule(scatterplot, 'pca', getDatamatrix = pcaMatrix, getThreedee = getThreedee, getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis, getShowLabels = getShowLabels, getPointSize = getPointSize, title = 'PCA plot', colorby = na.replace(selectColData()[[colorBy()]], 'N/A'))

scatterplot <- function(input, output, session, getDatamatrix, getThreedee = NULL, getXAxis = NULL, getYAxis = NULL, getZAxis = NULL, getShowLabels = NULL, getPointSize = NULL, 
    title = "", getLabels = reactive({
        rownames(getDatamatrix())
    }), colorby = NULL) {
    
    # If inputs are not provided, render controls to provide them
    
    if (is.null(getThreedee)) {
        output$controls <- renderUI({
            ns <- session$ns
            scatterplotcontrolsInput(ns("scatter"))
        })
        unpack.list(callModule(scatterplotcontrols, "scatter", getDatamatrix))
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
    
    # Make the marker object
    
    marker <- reactive({
        list(size = getPointSize())
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
    
    # Down to business: plot the scatter plot
    
    output$scatter <- renderPlotly({
        
        withProgress(message = "Rendering scatter plot", value = 0, {
            
            # Wait for the controls to render before we proceed
            
            validate(need(getXAxis(), FALSE))
            
            # Basic plot setup
            
            plotly_args <- list(x = xdata(), y = ydata(), z = zdata(), mode = "markers", text = getLabels(), hoverinfo = "text", marker = marker(), type = plotType(), 
                showlegend = showLegend(), color = colorby)
            p <- do.call(plotly::plot_ly, plotly_args)
            
            # Show specified labels
            
            if (getShowLabels()) {
                label_args <- list(x = xdata(), y = yLabData(), z = zdata(), mode = "text", text = getLabels(), type = plotType(), color = colorby, hoverinfo = "none", 
                  showlegend = FALSE)
                p <- do.call(add_trace, label_args)
            }
            
            # Apply the layout parameters
            
            withProgress(message = "Adjusting axis display", value = 0, {
                
                axis_layouts <- list(xaxis = list(title = colnames(getDatamatrix())[getXAxis()]), yaxis = list(title = colnames(getDatamatrix())[getYAxis()]), zaxis = list(title = colnames(getDatamatrix())[getZAxis()]))
                
                layoutArgs <- reactive({
                  la <- c(list(p, hovermode = "closest", title = title), axis_layouts)
                  
                  if (getThreedee()) {
                    la$scene <- axis_layouts
                    la$legend = list(y = 0.8)
                  }
                  la
                })
                
                do.call(plotly::layout, layoutArgs())
                
            })
        })
    })
    
} 
