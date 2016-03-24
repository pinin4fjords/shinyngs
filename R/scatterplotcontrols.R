#' Input function for scatterplotcontrols module
#' 
#' This module provides controls (2D/3D, axes etc) for scatter plots, which
#' may then be used by one or more instances of the scatterplot module.
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character()
#' @export
#'
#' @examples
#' scatterplotcontrolsInput('pca', allow_3d = FALSE) # for a 2D plot

scatterplotcontrolsInput <- function(id, allow_3d = TRUE) {
    
    ns <- NS(id)
    
    if (allow_3d) {
        inputs <- list(radioButtons(ns("threedee"), "Plot type", c(`3D` = TRUE, `2D` = FALSE), inline = TRUE))
    } else {
        inputs <- list(hiddenInput(ns("threedee"), FALSE))
    }
    
    c(inputs, list(uiOutput(ns("plotColumns")), checkboxInput(ns("showLabels"), "Show labels?"), sliderInput(ns("pointSize"), "Point size", min = 1, max = 20, value = 7)))
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
#' @export
#'
#' @examples
#' unpack.list(callModule(scatterplotcontrols, 'pca', pcaMatrix, x = 1, y = 2)) # To have fixed axes rather than user-selected

scatterplotcontrols <- function(input, output, session, getDatamatrix, x = NA, y = NA, z = NA) {
    
    output$plotColumns <- renderUI({
        withProgress(message = "Making scatter plot controls", value = 0, {
            
            validate(need(input$threedee, "Waiting for threedee"))
            
            ns <- session$ns
            vars <- structure(1:ncol(getDatamatrix()), names = colnames(getDatamatrix()))
            
            # Work out how many axes we need
            
            axes <- c(x = x, y = y)
            if (as.logical(input$threedee)) {
                axes$z <- z
            }
            
            # Make a select for each axis
            
            lapply(1:length(axes), function(n) {
                ax <- names(axes)[n]
                select <- selectInput(ns(paste0(ax, "Axis")), paste(ax, "axis"), vars, selected = ifelse(is.na(axes[n]), n, axes[n]))
                
                if (is.na(axes[n])) {
                  select
                } else {
                  shinyjs::hidden(select)
                }
                
            })
            
        })
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
        as.logical(input$showLabels)
    })
    
    getPointSize <- reactive({
        input$pointSize
    })
    
    list(getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis, getThreedee = getThreedee, getShowLabels = getShowLabels, getPointSize = getPointSize)
    
} 
