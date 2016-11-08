#' Input function for scatterplotcontrols module
#' 
#' This module provides controls (2D/3D, axes etc) for scatter plots, which
#' may then be used by one or more instances of the scatterplot module.
#'
#' @param id Submodule namespace
#' @param allow_3d Allow user to choose 3D plotting?
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character()
#'
#' @examples
#' scatterplotcontrolsInput('pca', allow_3d = FALSE) # for a 2D plot

scatterplotcontrolsInput <- function(id, allow_3d = TRUE, make_colors = FALSE) {
    
    ns <- NS(id)
    
    if (allow_3d) {
        inputs <- list(radioButtons(ns("threedee"), "Plot type", c(`3D` = TRUE, `2D` = FALSE), inline = TRUE))
    } else {
        inputs <- list(hiddenInput(ns("threedee"), FALSE))
    }
    
    if (make_colors) {
        inputs <- c(inputs, list(colormakerInput(ns("scatterplot"))))
    }
    
    c(inputs, list(uiOutput(ns("plotColumns")), checkboxInput(ns("showLabels"), "Show labels?"), sliderInput(ns("pointSize"), "Point size", min = 1, 
        max = 20, value = 5)))
}

#' Server function for scatterplotcontrols module
#' 
#' This module provides controls (2D/3D, axes etc) for scatter plots, which
#' may then be used by one or more instances of the scatterplot module.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param getDatamatrix Reactive expression that returns a matrix from which 
#' coulumn headers will be used to create axis select drop-downs. The same 
#' reactive should be supplied to the scatterplot module
#' @param x A value supplied for this parameter will cause a hidden field to 
#' be generated instead of a select, useful for scatter plots that don't need
#' the user to select axes (default: NA)
#' @param y A value supplied for this parameter will cause a hidden field to 
#' be generated instead of a select, useful for scatter plots that don't need
#' the user to select axes (default: NA)
#' @param z A value supplied for this parameter will cause a hidden field to 
#' be generated instead of a select, useful for scatter plots that don't need
#' the user to select axes (default: NA)
#'
#' @return output A list of reactives for accessing input values
#'
#' @examples
#' unpack.list(callModule(scatterplotcontrols, 'pca', pcaMatrix, x = 1, y = 2)) # To have fixed axes rather than user-selected

scatterplotcontrols <- function(input, output, session, getDatamatrix, x = NA, y = NA, z = NA, makeColors = NULL) {
    
    output$plotColumns <- renderUI({
        withProgress(message = "Making scatter plot controls", value = 0, {
            
            ns <- session$ns
            datamatrix <- getDatamatrix()
            vars <- structure(1:ncol(datamatrix), names = colnames(datamatrix))
            
            # Work out how many axes we need
            
            axes <- list(x = x, y = y)
            if (getThreedee()) {
                axes$z <- z
            }
            
            # Make a select for each axis
            
            axis_filters <- lapply(1:length(axes), function(n) {
                ax <- names(axes)[n]
                
                if (is.na(axes[n])) {
                  selectInput(ns(paste0(ax, "Axis")), paste(ax, "axis"), vars, selected = n)
                } else {
                  hiddenInput(ns(paste0(ax, "Axis")), axes[n])
                }
                
            })
            
        })
        axis_filters
    })
    
    # Provide accessor methods for inputs
    
    getXAxis <- reactive({
        validate(need(input$xAxis, FALSE))
        as.numeric(input$xAxis)
    })
    
    getYAxis <- reactive({
        validate(need(input$yAxis, FALSE))
        as.numeric(input$yAxis)
    })
    
    getZAxis <- reactive({
        if (getThreedee()) {
            validate(need(input$zAxis, FALSE))
            as.numeric(input$zAxis)
        } else {
            NULL
        }
    })
    
    getThreedee <- reactive({
        validate(need(input$threedee, FALSE))
        as.logical(input$threedee)
    })
    
    getShowLabels <- reactive({
        validate(need(input$threedee, "Waiting for showLabels"))
        as.logical(input$showLabels)
    })
    
    getPointSize <- reactive({
        validate(need(input$threedee, "Waiting for pointsize"))
        input$pointSize
    })
    
    reactives <- list(getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis, getThreedee = getThreedee, getShowLabels = getShowLabels, getPointSize = getPointSize)
    
    # If specified, make a palette for the specified number of colors
    
    if (!is.null(makeColors)) {
        reactives$getScatterPalette <- callModule(colormaker, "scatterplot", getNumberCategories = makeColors)
    }
    
    reactives
} 
