#' The UI input function of the differentialtable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'contrasts' slot of a ExploratorySummarizedExperimentList
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
    fieldSets(ns("fieldset"), list(contrasts = list(contrastsInput(ns("differential"))), select_assay_data = expression_filters, 
        export = simpletableInput(ns("differentialtable"))))
}

#' The output function of the differentialtable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'contrasts' slot of a ExploratorySummarizedExperimentList
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
    
    list(modalInput(ns("differentialtable"), "help", "help"), modalOutput(ns("differentialtable"), "Differential expression table", 
        includeMarkdown(system.file("inlinehelp", "differentialtable.md", package = packageName()))), htmlOutput(ns("differentialtable")), contrastsOutput(ns('differential')))
}

#' The server function of the differentialtable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'contrasts' slot of a ExploratorySummarizedExperimentList
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example). 
#' 
#' Essentially this funnction uses the \code{contrasts} module to group samples
#' and calculate fold changes, adding test statistics where available.
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
    
    selectmatrix_reactives <- callModule(selectmatrix, "expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = TRUE, 
        provide_all_genes = TRUE)
    unpack.list(selectmatrix_reactives)
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "differential", selectmatrix_reactives = selectmatrix_reactives, eselist = eselist, multiple = TRUE))
    
    # Pass the matrix to the simpletable module for display
    
    callModule(simpletable, "differentialtable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, 
        filename = "differential", rownames = FALSE)
} 
