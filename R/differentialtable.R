#' The UI input function of the differentialtable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'tests' slot of the ExploratorySummarizedExperiment
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
#' differentialtableInput('experiment', eselist)

differentialtableInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    fieldSets(ns("fieldset"), list(contrasts = list(contrastsInput(ns("differential"))), select_assay_data = expression_filters, export = simpletableInput(ns("differentialtable"))))
}

#' The output function of the differentialtable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'tests' slot of the ExploratorySummarizedExperiment
#' 
#' Leverages the \code{simpletable} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' differentialtableOutput('experiment')

differentialtableOutput <- function(id) {
    ns <- NS(id)
    
    htmlOutput(ns("differentialtable"))
}

#' The server function of the differentialtable module
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
#' callModule(differentialtable, 'differentialtable', eselist)

differentialtable <- function(input, output, session, eselist) {
    
    # Render the output area - and provide an input-dependent title
    
    output$differentialtable <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("differentialtable"), tabletitle = paste("Differential expression in assay", getAssay(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = TRUE, provide_all_genes = TRUE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "differential", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = FALSE))
    
    # Pass the matrix to the simpletable module for display
    
    callModule(simpletable, "differentialtable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, filename = "differential", 
        rownames = FALSE)
} 
