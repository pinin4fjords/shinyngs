#' The UI input function of the sampleselect module
#'  
#' This module provides controls for selecting matrix columns by sample or 
#' group name.
#' 
#' This will generally not be called directly, but by other modules such as the
#' selectmatrix module.
#'
#' @param id Submodule namespace
#' @param se StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' sampleselectInput(ns('heatmap'))

sampleselectInput <- function(id, se, select_samples = TRUE) {
    
    ns <- NS(id)
    
    if (select_samples) {
        
        # If grouping variables have been supplied we can use them to define sample selection
        
        selectby <- "name"
        if ("group_vars" %in% names(metadata(se))) {
            selectby <- c(selectby, "group")
            default_groupvar <- se$group_vars[1]
            if ("default_groupvar" %in% names(metadata(se))) {
                default_groupvar <- metadata(se)$default_groupvar
            }
        }
        
        # We can select by sample in any case
        
        inputs <- list(h5("Select samples/ columns"), selectInput(ns("sampleSelect"), "Select samples by", selectby, selected = selectby[length(selectby)]), 
            conditionalPanel(condition = paste0("input['", ns("sampleSelect"), "'] == 'name' "), checkboxGroupInput(ns("samples"), "Samples:", colnames(se), 
                selected = colnames(se), inline = TRUE)))
        
        # Add in group selection if relevant
        
        if ("group_vars" %in% names(metadata(se))) {
            inputs <- pushToList(inputs, conditionalPanel(condition = paste0("input['", ns("sampleSelect"), "'] == 'group' "), selectInput(ns("sampleGroupVar"), 
                "Define groups by:", structure(metadata(se)$group_vars, names = prettifyVariablename(metadata(se)$group_vars)), selected = metadata(se)$default_groupvar), 
                uiOutput(ns("groupSamples"))))
        }
        
    } else {
        inputs <- list(hiddenInput(ns("sampleSelect"), "all"))
    }
    
    tagList(inputs)
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
#' @param getExperiment Reactive expression which returns a
#' StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#'
#' @return output A list of reactive functions for interrogating the selected
#' samples/ columns.
#'
#' @keywords shiny
#' 
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', getExperiment)

sampleselect <- function(input, output, session, getExperiment) {
    
    getSummaryType <- callModule(summarisematrix, "summarise")
    
    observe({
        se <- getExperiment()
    })
    
    # Render the sampleGroupVal() element based on sampleGroupVar
    
    output$groupSamples <- renderUI({
        if (input$sampleSelect == "group" && "group_vars" %in% names(metadata(getExperiment()))) {
            validate(need(input$sampleGroupVar, FALSE))
            group_values <- as.character(unique(se[[isolate(input$sampleGroupVar)]]))
            ns <- session$ns
            
            list(checkboxGroupInput(ns("sampleGroupVal"), "Groups", group_values, selected = group_values), summarisematrixInput(ns("summarise")))
        }
    })
    
    # Output a reactive so that other modules know whether we've selected by sample or group
    
    getSampleSelect <- reactive({
        input$sampleSelect
    })
    
    # Return summary type
    
    getSampleGroupVar <- reactive({
        input$sampleGroupVar
    })
    
    
    # Reactive expression for selecting the specified columns
    
    selectSamples <- reactive({
        withProgress(message = "Selecting samples", value = 0, {
            se <- getExperiment()
            
            validate(need(!is.null(input$sampleSelect), "Waiting for form to provide sampleSelect"))
            
            if (input$sampleSelect == "all") {
                return(colnames(se))
            } else {
                
                validate(need(!is.null(input$samples), "Waiting for form to provide samples"))
                
                if ("group_vars" %in% names(metadata(se))) {
                  validate(need(!is.null(input$sampleGroupVal), FALSE))
                }
                
                if (input$sampleSelect == "name") {
                  return(input$samples)
                } else {
                  
                  # Any NA in the colData will become string '' via the inputs, so make sure we consider that when matching
                  
                  samplegroups <- as.character(se[[isolate(input$sampleGroupVar)]])
                  samplegroups[is.na(samplegroups)] <- ""
                  
                  return(colnames(se)[samplegroups %in% input$sampleGroupVal])
                }
                
            }
        })
    })
    
    list(selectSamples = selectSamples, getSampleGroupVar = getSampleGroupVar, getSummaryType = getSummaryType, getSampleSelect = getSampleSelect)
} 
