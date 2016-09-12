#' The UI input function of the rowmetatable module
#' 
#' This module produces a simple table of the row metadata (accessed via 
#' \code{mcols}) in a SummarizedExperiment object. If more than one of these 
#' objects were specified, a select box should appear to pick the desired one for display.
#' 
#' Leverages the \code{simpletable} module
#' 
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @return output An HTML tag object that can be rendered as HTML using 
#'   as.character()
#'   
#' @keywords shiny
#'   
#' @examples
#' experimentableInput('experiment', eselist)

rowmetatableInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    description = "This is the metadata associated with the rows (e.g. genes) of this study."
    
    list(selectmatrixInput(ns("rowmetatable"), eselist = eselist), simpletableInput(ns("rowmetatable"), tabletitle = "Annotation"))
}

#' The output function of the rowmetatable module
#' 
#' This module produces a simple table of the \code{colData()} in an 
#' ExploratorySummarizedExperiment object. If more than one of these objects
#' were specified, a select box should appear to pick the desired one for
#' display.
#' 
#' Leverages the \code{simpletable} module
#' 
#' @param id Module namespace
#'   
#' @return output An HTML tag object that can be rendered as HTML using 
#'   as.character()
#'   
#' @keywords shiny
#'   
#' @examples
#' rowmetatableOutput('experiment')

rowmetatableOutput <- function(id) {
    ns <- NS(id)
    list(modalInput(ns("rowmetatable"), "help", "help"), modalOutput(ns("rowmetatable"), "Experimental data table", includeMarkdown(system.file("inlinehelp", 
        "rowmetatable.md", package = packageName()))), simpletableOutput(ns("rowmetatable"), tabletitle = "Row metadata"))
}

#' The server function of the rowmetatable module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example). Essentially this just passes the results of \code{colData()} 
#' applied to the specified SummarizedExperiment object to the 
#' \code{simpletable} module
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @keywords shiny
#'   
#' @examples
#' callModule(rowmetatable, 'rowmetatable', eselist)

rowmetatable <- function(input, output, session, eselist) {
    
    getRowMeta <- reactive({
        meta <- getAnnotation()
        meta
    })
    
    getLinkedRowMeta <- reactive({
        meta <- getRowMeta()
        colnames(meta) <- prettifyVariablename(colnames(meta))
        linkMatrix(meta, eselist@url_roots)
    })
    
    unpack.list(callModule(selectmatrix, "rowmetatable", eselist, select_assays = FALSE, select_samples = FALSE, select_genes = FALSE))
    callModule(simpletable, "rowmetatable", displayMatrix = getLinkedRowMeta, downloadMatrix = getRowMeta, filename = "rowmeta", rownames = TRUE, pageLength = 10)
}
