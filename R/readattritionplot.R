#' Input function of the \code{readattritionplot} module 
#' 
#' Display a read attrition table and plot showing where reads were lost during
#' trimming, mapping etc
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @return A list of controls that can be added to a UI definition
#' @export

readattritionplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    fieldSets(ns("fieldsets"), list(bar_plot = barplotInput(ns("barplot"), default_mode = "overlay"), export = simpletableInput(ns("readattr"))))
    
}

#' Output function of the \code{readattritionplot} module 
#' 
#' Display a read attrition table and plot showing where reads were lost during
#' trimming, mapping etc
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @return A list of elements that can be included in a panel
#' @export

readattritionplotOutput <- function(id, eselist) {
    ns <- NS(id)
    
    list(modalInput(ns("readattritionplot"), "help", "help"), modalOutput(ns("readattritionplot"), "Read distribution plot", includeMarkdown(system.file("inlinehelp", 
        "readattritionplot.md", package = packageName()))), h3("Read attrition report"), barplotOutput(ns("barplot")), h4("Tag counts for each genomic feature type"), 
        simpletableOutput(ns("readattr")))
}

#' Server function of the \code{readattritionplot} module 
#' 
#' Display a read attrition table and plot showing where reads were lost during
#' trimming, mapping etc
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @export

readattritionplot <- function(input, output, session, eselist) {
    
    getPlotmatrix <- reactive({
        t(getAttritionTable())
    })
    
    getAttritionTable <- reactive({
        plotmatrix <- eselist@read_attrition
        colnames(plotmatrix) <- prettifyVariablename(colnames(plotmatrix))
        plotmatrix
    })
    
    callModule(barplot, "barplot", getPlotmatrix = getPlotmatrix, getYLabel = reactive({
        "Reads"
    }))
    callModule(simpletable, "readattr", displayMatrix = getAttritionTable, filename = "read_attrition", rownames = TRUE)
    
} 
