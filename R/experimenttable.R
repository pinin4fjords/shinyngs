#' The UI input function of the experimenttable module
#' 
#' This module produces a simple table of the \code{colData()} in a 
#' SummarizedExperiment object. If more than one of these objects were 
#' specified, a select box should appear to pick the desired one for display.
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

experimenttableInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    description = "This is the metadata associated with the experimental samples of this study."
    
    if (length(eselist) == 1) {
        tagList(hiddenInput(ns("experiment"), names(eselist)[1]), fieldSets(ns("fieldset"), list(export = simpletableInput(ns("experimenttable"), "Experiment", description))))
    } else {
        fieldSets(ns("fieldset"), list(experiment = selectInput(ns("experiment"), "Experiment", names(eselist)), export = simpletableInput(ns("experimenttable"), "experiment", description)))
    }
}

#' The output function of the experimenttable module
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
#' experimenttableOutput('experiment')

experimenttableOutput <- function(id) {
    ns <- NS(id)
    list(modalInput(ns("experimenttable"), "help", "help"), modalOutput(ns("experimenttable"), "Experimental data table", includeMarkdown(system.file("inlinehelp", "experimenttable.md", 
        package = packageName()))), simpletableOutput(ns("experimenttable"), tabletitle = "Experimental data"))
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
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @keywords shiny
#'   
#' @examples
#' callModule(experimenttable, 'experimenttable', eselist)

experimenttable <- function(input, output, session, eselist) {
    
    getExperiment <- reactive({
        experiment <- data.frame(colData(eselist[[input$experiment]]))
        colnames(experiment) <- prettifyVariablename(colnames(experiment))
        experiment
    })
    
    callModule(simpletable, "experimenttable", displayMatrix = getExperiment, filename = "experiment", rownames = TRUE)
}
