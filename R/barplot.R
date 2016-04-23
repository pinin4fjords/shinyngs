#' Input function of the \code{barplot} module 
#' 
#' Display grouped, stacked or overlaid bars for a matrix
#' 
#'
#' @param id Submodule namespace
#' @param default_mode Default bar mode
#'
#' @return A list of controls that can be added to a UI definition
#' @export

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
#' Display grouped, stacked or overlaid bars for a matrix
#' 
#'
#' @param id Submodule namespace
#'
#' @return A list of elements that can be included in a panel
#' @export

barplotOutput <- function(id) {
    ns <- NS(id)
    
    list(plotlyOutput(ns("barPlot")))
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
#'
#' @export

barplot <- function(input, output, session, getPlotmatrix, getYLabel, barmode = "stack") {
    
    # If we're doing an overlay plot, let's re-order the rows such that we've a better chance of seeing each group
    
    formatPlotMatrix <- reactive({
        pm <- getPlotmatrix()
        if (input$barMode == "overlay") {
            pm <- pm[order(rowMeans(pm)), ]
        }
        pm
    })
    
    # Render the plot
    
    output$barPlot <- renderPlotly({
        plotdata <- reshape2::melt(formatPlotMatrix())
        plot_ly(plotdata, x = Var2, y = value, group = Var1, type = "bar", evaluate = TRUE) %>% layout(barmode = input$barMode, xaxis = list(title = " "), yaxis = list(title = getYLabel()), 
            evaluate = TRUE) %>% config(showLink = TRUE)
    })
} 
