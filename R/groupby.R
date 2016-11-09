#' The UI function of the groupby module
#' 
#' The groupby module provides a UI element to choose from the 
#' \code{group_vars} in a SummarizedExperment. Useful for coloring in a PCA etc
#'
#' @param id Submodule namespace
#' @param color Provide coloring functionality for groups?
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' groupbyInput(ns('heatmap'))

groupbyInput <- function(id, color = TRUE) {
    ns <- NS(id)
    
    fields <- list(uiOutput(ns("groupby_fields")))
    
    if (color) {
        fields <- pushToList(fields, colormakerInput(ns("groupby")))
    }
    
    fields
}

#' The server function of the groupby module
#' 
#' The groupby module provides a UI element to choose from the 
#' \code{group_vars} in a SummarizedExperment. Useful for coloring in a PCA etc
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param group_label A label for the grouping field
#' @param multiple Produces a checkbox group if true, a select box if false
#' @param selectColData Reactive returning an experiment matrix, probably 
#'   derived from the \code{\link{selectmatrix}} module.
#'   
#' @return output A list of reactive functions which will be used by other 
#' modules.
#'   
#' @keywords shiny
#'   
#' @examples
#' geneset_functions <- callModule(groupby, 'heatmap', getExperiment)

groupby <- function(input, output, session, eselist, group_label = "Group by", multiple = FALSE, selectColData = NULL, 
    isDynamic = reactive({
        TRUE
    })) {
    
    getPalette <- callModule(colormaker, "groupby", getNumberCategories = getNumberCategories)
    
    # Choose a default grouping variable, either the one specified or the first
    
    getDefaultGroupby <- reactive({
        if (multiple) {
            eselist@group_vars
        } else {
            if (length(eselist@default_groupvar) > 0) {
                eselist@default_groupvar
            } else {
                eselist@group_vars[1]
            }
        }
    })
    
    # Render function for the field
    
    output$groupby_fields <- renderUI({
        
        withProgress(message = "Rendering group by", value = 0, {
            ns <- session$ns
            
            if (length(eselist@group_vars) > 0) {
                
                dynamic <- isDynamic()
                
                group_options <- structure(eselist@group_vars, names = prettifyVariablename(eselist@group_vars))
                
                if (multiple) {
                  groupinput <- checkboxGroupInput(ns("groupby"), group_label, group_options, selected = group_options, 
                    inline = TRUE)
                } else {
                  groupinput <- selectInput(ns("groupby"), group_label, group_options, selected = getDefaultGroupby())
                }
                
                if (!dynamic) {
                  groupinput <- shinyjs::hidden(groupinput)
                }
                
                groupinput
            } else {
                hiddenInput(ns("groupby"), "NULL")
            }
        })
    })
    
    # Return a reactive that retrieves the field value
    
    getGroupby <- reactive({
        validate(need(input$groupby, "waiting for form to provide groupby"))
        if (input$groupby[1] == "NULL") {
            NULL
        } else {
            input$groupby
        }
    })
    
    # Get the number of categories given an input experiment matrixa and the selected grouping variable.
    
    getNumberCategories <- reactive({
        group_by <- getGroupby()
        coldata <- selectColData()
        
        if (!is.null(group_by) && !is.null(coldata)) {
            length(unique(coldata[[group_by]]))
        }
    })
    
    list(getGroupby = getGroupby, getNumberCategories = getNumberCategories, getPalette = getPalette)
} 
