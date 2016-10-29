#' The input function of the gene plotdownload module
#' 
#' The plotdownload module provides export functionality for panels with plots.
#' This will generally not be called directly, but by other modules
#'
#' @param id Submodule namespace
#' @param label Label for the download button
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' plotdownloadInput(ns('heatmap'))

plotdownloadInput <- function(id, label = "Plot") {
    
    ns <- NS(id)
    
    downloadButton(ns("plotdownload"), label)
}

#' The server function of the gene set module
#' 
#' The plotdownload module provides export functionality for panels with plots.
#' This will generally not be called directly, but by other modules
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param makePlot A reactive for generating the plot
#' @param filename A filename (default = 'plot.png')
#' @param plotHeight A number or reactive for calculating the height of the plot
#' @param plotWidth A number or reactive for calculating the width of the plot
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(plotdownload, 'heatmap', makePlot = plotHeatmap, filename = 'heatmap.png', plotHeight = plotHeight, plotWidth = plotWidth)

plotdownload <- function(input, output, session, makePlot, filename = "plot.png", plotHeight, plotWidth) {
    
    if (is.reactive(plotHeight)) {
        plotHeight <- as.numeric(plotHeight())
    }
    
    if (is.reactive(plotWidth)) {
        plotWidth = as.numeric(plotWidth())
    }
    
    output$plotdownload <- downloadHandler(filename = filename, content = function(file) {
        png(file, height = plotHeight, width = plotWidth, units = "px")
        print(makePlot())
        dev.off()
    })
    
} 
