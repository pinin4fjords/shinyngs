#' The UI input function of the assaydatatable module
#'  
#' This module produces a simple table of the \code{colData()} in a
#' SummarizedExperiment object. If more than one of these objects were 
#' specified, a select box should appear to pick the desired one for display.
#' 
#' Leverages the \code{simpletable} module
#'
#' @param id Submodule namespace
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' assaydatatableInput('experiment', ses)

assaydatatableInput <- function(id, ses) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("expression"), ses)
    fieldSets(ns("fieldset"), list(select_assay_data = expression_filters, export = simpletableInput(ns("assaydatatable"))))
}

#' The output function of the assaydatatable module
#' 
#' This module produces a simple table of the \code{colData()} in a
#' SummarizedExperiment object. If more than one of these objects were 
#' specified, a select box should appear to pick the desired one for display.
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
#' assaydatatableOutput('experiment')

assaydatatableOutput <- function(id) {
    ns <- NS(id)
    
    htmlOutput(ns("assaydatatable"))
    
    # simpletableOutput(ns('assaydatatable'), tabletitle = 'Expression data')
}

#' The server function of the assaydatatable module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example). Essentially this just passes the results of \code{colData()} 
#' applied to the specified SummarizedExperiment object to the 
#' \code{simpletable} module
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(assaydatatable, 'assaydatatable', ses)

assaydatatable <- function(input, output, session, ses) {
    
    # Render the output area - and provide an input-dependent title
    
    output$assaydatatable <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("assaydatatable"), tabletitle = paste("Assay data", getAssay(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", ses, var_n = 1000, select_genes = TRUE, provide_all_genes = TRUE))
    
    # Pass the matrix to the simpletable module for display
    
    callModule(simpletable, "assaydatatable", downloadMatrix = selectLabelledMatrix, displayMatrix = selectLabelledLinkedMatrix, filename = getAssay(), rownames = FALSE)
} 
