#' The UI function of the groupby module
#' 
#' The groupby module provides a UI element to choose from the group_vars in
#' a SummarizedExperment. Useful for coloring in a PCA etc
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' groupbyInput(ns('heatmap'))

groupbyInput <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns("groupby"))
}

#' The server function of the groupby module
#' 
#' The groupby module provides a UI element to choose from the group_vars in a
#' SummarizedExperment. Useful for coloring in a PCA etc
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param group_label A label for the grouping field
#' @param multiple Produces a checkbox group if true, a select box if false
#'   
#' @return output A list of two reactive functions: getPathwayNames() and 
#'   getPathwayGenes() which will be used by other modules.
#'   
#' @keywords shiny
#'   
#' @examples
#' geneset_functions <- callModule(groupby, 'heatmap', getExperiment)

groupby <- function(input, output, session, eselist, group_label = "Group by", multiple = FALSE) {
    
    # Choose a default grouping variable, either the one specified or the first
    
    getDefaultGroupby <- reactive({
        if (length(eselist@default_groupvar) > 0) {
            eselist@default_groupvar
        } else {
            eselist@group_vars[1]
        }
    })
    
    # Render function for the field
    
    output$groupby <- renderUI({
        
        withProgress(message = "Rendering group by", value = 0, {
            ns <- session$ns
            
            if (length(eselist@group_vars) > 0) {
                
                group_options <- structure(eselist@group_vars, names = prettifyVariablename(eselist@group_vars))
                
                if (multiple) {
                  checkboxGroupInput(ns("groupby"), group_label, group_options, selected = group_options, inline = TRUE)
                } else {
                  selectInput(ns("groupby"), group_label, group_options, selected = getDefaultGroupby())
                }
            } else {
                hiddenInput(ns("groupby"), "NULL")
            }
        })
    })
    
    # Return a reactive that retrieves the field value
    
    reactive({
        validate(need(input$groupby, "waiting for form to provide groupby"))
        if (input$groupby == "NULL") {
            NULL
        } else {
            input$groupby
        }
    })
} 
