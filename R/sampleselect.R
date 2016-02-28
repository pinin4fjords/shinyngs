#' The UI input function of the sampleselect module
#'  
#' This module provides controls for selecting matrix columns by sample or 
#' group name.
#' 
#' This will generally not be called directly, but by other modules such as the
#' selectmatrix module.
#'
#' @param id Submodule namespace
#' @param group_vars The variables from the structured experiment that should
#' be used to control sample grouping in the plot
#' @param The default grouping variable to use
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' sampleselectInput(ns('heatmap'))

sampleselectInput <- function(id, group_vars, default_groupvar) {
    
    ns <- NS(id)
    
    tagList(h4("Select samples/ columns"), selectInput(ns("sampleSelect"), "Select samples by", c("name", "group"), selected = "group"), conditionalPanel(condition = paste0("input['", 
        ns("sampleSelect"), "'] == 'name' "), checkboxGroupInput(ns("samples"), "Samples:", colnames(se), selected = colnames(se), inline = TRUE)), 
        conditionalPanel(condition = paste0("input['", ns("sampleSelect"), "'] == 'group' "), selectInput(ns("sampleGroupVar"), "Define groups by:", 
            group_vars, selected = default_groupvar), uiOutput(ns("groupSamples"))))
}

#' The server function of the sampleselect module
#'  
#' This module provides controls for selecting matrix columns by sample or 
#' group name.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param se StructuredExperiment object with assay and experimental data
#'
#' @return output A list of reactive functions for interrogating the selected
#' samples/ columns.
#'
#' @keywords shiny
#' 
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', se)

sampleselect <- function(input, output, session, se) {
    
    # Render the sampleGroupVal() element based on sampleGroupVar
    
    output$groupSamples <- renderUI({
        validate(need(input$sampleGroupVar, FALSE))
        group_values <- as.character(unique(se[[isolate(input$sampleGroupVar)]]))
        ns <- session$ns
        checkboxGroupInput(ns("sampleGroupVal"), "Groups", group_values, selected = group_values)
    })
    
    # Reactive expression for selecting the specified columns
    
    reactive({
        
        validate(need(!is.null(input$samples), "Waiting for form to provide samples"), need(!is.null(input$sampleGroupVal), "Waiting in sampleselect for form to provide sampleGroupVal"))
        
        if (input$sampleSelect == "name") {
            return(input$samples)
        } else {
            return(colnames(se)[se[[isolate(input$sampleGroupVar)]] %in% input$sampleGroupVal])
        }
    })
} 
