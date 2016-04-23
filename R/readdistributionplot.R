#' Input function of the \code{readdistributionplot} module 
#' 
#' Display a read distribution table and plot for genomic feature counts 
#' (exons, introns etc) vs sample, as might be generated using RSeQC.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @return A list of controls that can be added to a UI definition
#' @export

readdistributionplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    fieldSets(ns("fieldsets"), list(bar_plot = barplotInput(ns("barplot"), default_mode = "stack"), export = simpletableInput(ns("readdist"))))
    
}

#' Output function of the \code{readdistributionplot} module 
#' 
#' Display a read distribution table and plot for genomic feature counts 
#' (exons, introns etc) vs sample, as might be generated using RSeQC.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @return A list of elements that can be included in a panel
#' @export

readdistributionplotOutput <- function(id, eselist) {
    ns <- NS(id)
    
    list(modalInput(ns("readdistributionplot"), "help", "help"), modalOutput(ns("readdistributionplot"), "Read distribution plot", includeMarkdown(system.file("inlinehelp", 
        "readdistributionplot.md", package = packageName()))), h3("Read distribution plot"), barplotOutput(ns("barplot")), h4("Plot data"), simpletableOutput(ns("readdist")))
}

#' Server function of the \code{readdistributionplot} module 
#' 
#' Display a read distribution table and plot for genomic feature counts 
#' (exons, introns etc) vs sample, as might be generated using RSeQC.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @export

readdistributionplot <- function(input, output, session, eselist) {
    
    getPlotmatrix <- reactive({
        t(eselist@read_distribution)
    })
    
    callModule(barplot, "barplot", getPlotmatrix = getPlotmatrix, getYLabel = reactive({
        "Tags"
    }))
    callModule(simpletable, "readdist", displayMatrix = getPlotmatrix, filename = "read_distribution", rownames = TRUE)
    
} 
