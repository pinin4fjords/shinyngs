#' The UI input function of the experimenttable module
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
#' experimentableInput('experiment', ses)

experimenttableInput <- function(id, ses) {
    
    ns <- NS(id)
    
    description = "This is the metadata associated with the experimental samples of this study."
    
    if (length(ses) == 1) {
        tagList(hiddenInput(ns("experiment"), names(ses)[1]), fieldSets(ns("fieldset"), list(export = simpletableInput(ns("experimenttable"), description))))
    } else {
        fieldSets(ns("fieldset"), list(experiment = selectInput(ns("experiment"), "Experiment", names(ses)), export = simpletableInput(ns("experimenttable"), 
            description)))
    }
}

#' The output function of the experimenttable module
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
#' experimenttableOutput('experiment')

experimenttableOutput <- function(id) {
    ns <- NS(id)
    simpletableOutput(ns("experimenttable"), tabletitle = "Experimental data")
}

#' The server function of the experimenttable module
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
#' callModule(experimenttable, 'experimenttable', ses)

experimenttable <- function(input, output, session, ses) {
    
    getExperiment <- reactive({
        experiment <- data.frame(colData(ses[[input$experiment]]))
        colnames(experiment) <- prettifyVariablename(colnames(experiment))
        experiment
    })
    
    callModule(simpletable, "experimenttable", displayMatrix = getExperiment, filename = "experiment")
} 
