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

barplotInput <- function(id, default_mode = "stack") {
    ns <- NS(id)
    
    list(selectInput(ns("barMode"), "Mode", choices = c("group", "stack", "overlay"), selected = default_mode))
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
    
    output$barPlot <- renderPlotly({
        plotdata <- reshape2::melt(getPlotmatrix())
        plot_ly(plotdata, x = Var2, y = value, group = Var1, type = "bar", evaluate = TRUE) %>% layout(barmode = input$barMode, xaxis = list(title = " "), yaxis = list(title = getYLabel()), 
            evaluate = TRUE) %>% config(showLink = TRUE)
    })
} 
