#' The UI input function of the assaydatatable module
#' 
#' This module displays the content of the currently selected experiment and 
#' assay, also allowing grouping by mean etc.
#' 
#' Leverages the \code{simpletable} module
#' 
#' @param id Submodule namespace
#' @param eses List of ExploratorySummarizedExperiment objects with assay and
#'   experimental data
#'   
#' @return output An HTML tag object that can be rendered as HTML using 
#'   as.character()
#'   
#' @keywords shiny
#'   
#' @examples
#' assaydatatableInput('experiment', eses)

assaydatatableInput <- function(id, eses) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("expression"), eses)
    fieldSets(ns("fieldset"), list(select_assay_data = expression_filters, export = simpletableInput(ns("assaydatatable"))))
}

#' The output function of the assaydatatable module
#' 
#' This module displays the content of the currently selected experiment and 
#' assay, also allowing grouping by mean etc.  
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
#' @param eses List of structuredExperiment objects with assay and experimental
#' data
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(assaydatatable, 'assaydatatable', eses)

assaydatatable <- function(input, output, session, eses) {
    
    # Render the output area - and provide an input-dependent title
    
    output$assaydatatable <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("assaydatatable"), tabletitle = paste("Assay data", getAssay(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eses, var_n = 1000, select_genes = TRUE, provide_all_genes = TRUE))
    
    # Pass the matrix to the simpletable module for display
    
    callModule(simpletable, "assaydatatable", downloadMatrix = selectLabelledMatrix, displayMatrix = selectLabelledLinkedMatrix, filename = getAssay(), 
        rownames = FALSE)
} 
