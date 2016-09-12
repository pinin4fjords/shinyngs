#' Input function of the \code{barplot} module 
#' 
#' Generic module to isplay grouped, stacked or overlaid bars for a matrix
#' 
#' @param id Submodule namespace
#' @param default_mode Default bar mode
#' @param allow_select Allow user to choose stack mode?
#'
#' @return A list of controls that can be added to a UI definition

barplotInput <- function(id, default_mode = "stack", allow_select = TRUE) {
    ns <- NS(id)
    
    if (allow_select) {
        selectInput(ns("barMode"), "Mode", choices = c("group", "stack", "overlay"), selected = default_mode)
    } else {
        hiddenInput(ns("barMode"), default_mode)
    }
}

#' Output function of the \code{barplot} module 
#' 
#' Generic module to isplay grouped, stacked or overlaid bars for a matrix
#' 
#' @param id Submodule namespace
#' @param heigth Height of the plotting space in px
#'
#' @return A list of elements that can be included in a panel

barplotOutput <- function(id, height = "400") {
    ns <- NS(id)
    
    list(plotlyOutput(ns("barPlot"), height = paste0(height, "px")))
}

#' Server function of the \code{barplot} module 
#' 
#' Display grouped, stacked or overlaid bars for a matrix
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param getPlotmatrix Reactive supplying a matrix to plot
#' @param getYLabel Reactive supplying the Y axis label
#' @param barmode Bar mode: 'stack', 'group' or 'overlay'

barplot <- function(input, output, session, getPlotmatrix, getYLabel, barmode = "stack") {
    
    # If we're doing an overlay plot, let's re-order the rows such that we've a better chance of seeing each group
    
    formatPlotMatrix <- reactive({
        pm <- getPlotmatrix()
        
        validate(need(input$barMode, "Waiting for bar mode"))
        
        if (input$barMode == "overlay") {
            pm <- pm[order(rowMeans(pm), decreasing = TRUE), ]
        }
        pm
    })
    
    # Render the plot
    
    output$barPlot <- renderPlotly({
        fpm <- formatPlotMatrix()
        plotdata <- reshape2::melt(fpm)
        plotdata %>% plot_ly(x = ~Var2, y = ~value, color = ~Var1, type = "bar") %>% layout(margin = list(b = 100), barmode = input$barMode, xaxis = list(title = " "), 
            yaxis = list(title = getYLabel())) %>% config(showLink = TRUE)
    })
}
