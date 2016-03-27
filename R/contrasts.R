#' The input function of the contrasts module
#' 
#' This module provides the form elements to control contrasts used in e.g. 
#' differential expression panels.
#'
#' @param id Submodule namespace
#' @param default_min_foldchange default value for the fold change filter
#' @param default_max_q default value for the q value filter
#' @param allow_filtering Provide the filtering fields? Can be disabled to
#' produce unfiltered contrasts tables.
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' contrastsInput('test')

contrastsInput <- function(id, default_min_foldchange = 2, default_max_q = 0.1, allow_filtering = TRUE) {
    
    ns <- NS(id)
    
    inputs <- list(uiOutput(ns("contrasts")))

    if (allow_filtering){

      inputs <- pushToList(inputs, checkboxInput(ns('filterRows'), 'Filter rows', TRUE))
      inputs <- pushToList(inputs, conditionalPanel(
           condition = paste0("input['", ns("filterRows"), "'] == true"),
           numericInput(ns("fcMin"), "Minimum absolute fold change", value = default_min_foldchange),
           numericInput(ns("qvalMax"), "Maximum false discovery rate", value = default_max_q)
        ))
    }else{
      inputs <- pushToList(inputs, shinyjs::hidden(checkboxInput(ns('filterRows'), 'Filter rows', FALSE)))
    }

    pushToList(inputs, summarisematrixInput(ns("contrasts"), allow_none = FALSE))
}

#' The server function of the contrasts module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param getExperiment Reactive for getting the selected experiment. Probably 
#' get this from the \code{selectmatrix} module
#' @param selectMatrix Reactive for generating a matrix to do comparisons with
#' @param getAssay Reactive for fetching the current assay. 
#' @param multiple Allow selection of multiple contrasts?
#' @param show_controls Show the controls for contrast selection? 
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(contrasts, 'differential', getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE)

contrasts <- function(input, output, session, getExperiment, selectMatrix, getAssay, multiple = FALSE, show_controls = TRUE) {
    
    getSummaryType <- callModule(summarisematrix, "contrasts")
    
    # Render the controls depending on currently selected experiment etc.
    
    output$contrasts <- renderUI({
        
        ns <- session$ns
        
        contrasts <- getAllContrasts()
        
        if (!is.null(contrasts)) {
            
            if (multiple) {
                cont_control <- checkboxGroupInput(ns("contrasts"), "Contrast(s):", contrasts, selected = contrasts)
            } else {
                cont_control <- selectInput(ns("contrasts"), "Contrast(s):", contrasts)
            }
            
            if (!show_controls) {
                cont_control <- shinyjs::hidden(cont_control)
            }
            cont_control
        }
        
        
    })
    
    # Get all the contrasts the user specified in their StructuredExperiment- if any
    
    getAllContrasts <- reactive({
        se <- getExperiment()
        
        if ("contrasts" %in% names(metadata(se))) {
            contrasts <- metadata(se)$contrasts
            
            structure(1:length(contrasts), names = lapply(contrasts, function(x) paste(prettifyVariablename(x[1]), paste(x[3], x[2], sep = " vs "), sep = ": ")))
        } else {
            NULL
        }
    })
    
    # Get the actual contrasts to which the numbers from the interface pertain
    
    getContrasts <- reactive({
        se <- getExperiment()
        metadata(se)$contrasts[getSelectedContrasts()]
    })
    
    getSelectedContrasts <- reactive({
      validate(need(input$contrasts, 'Waiting for contrasts'))
        as.numeric(input$contrasts)
    })
    
    getSelectedContrastNames <- reactive({
        names(getAllContrasts())[getSelectedContrasts()]
    })
    
    # Generate the summary statistic (probably mean) for column groups as defined by the possible contrasts. Other functions can then pick from this output and calculate fold
    # changes etc.
    
    getSummaries <- reactive({
        
        contrasts <- metadata(se)$contrasts[getAllContrasts()]
        
        contrast_variables <- unique(unlist(lapply(contrasts, function(x) x[1])))
        names(contrast_variables) <- contrast_variables
        
        withProgress(message = paste("Calculating summaries by", getSummaryType()), value = 0, {
            summaries <- lapply(contrast_variables, function(cv) summarizeMatrix(selectMatrix(), data.frame(colData(getExperiment()))[[cv]], getSummaryType()))
        })
        
        summaries
    })
    
    # Main function for returning the table of contrast information. Means, fold changes calculated on the fly, p/q values must be supplied in a 'tests' slot of the metadata.
    
    contrastsTables <- reactive({
        matrix <- selectMatrix()
        se <- getExperiment()
        
        summaries <- getSummaries()
        
        withProgress(message = "Calculating summary data", value = 0, {
            
            contrast_tables <- lapply(getSelectedContrasts(), function(c) {
                
                cont <- metadata(se)$contrasts[[c]]
                
                smry1 <- summaries[[cont[1]]][, cont[2]]
                smry2 <- summaries[[cont[1]]][, cont[3]]
                
                ct <- data.frame(round(smry1, 2), round(smry2, 2), round(foldChange(smry1, smry2), 2))
                names(ct) <- c(cont[2], cont[3], "Fold change")
                
                
                if ("tests" %in% names(metadata(se)) && getAssay() %in% names(metadata(se)$tests)) {
                  pvals <- metadata(se)$tests[[getAssay()]]$pval
                  qvals <- metadata(se)$tests[[getAssay()]]$qval
                  
                  ct[["p value"]] <- round(pvals[match(rownames(ct), rownames(pvals)), c], 5)
                  ct[["q value"]] <- round(qvals[match(rownames(ct), rownames(qvals)), c], 5)
                  
                } else {
                  ct[["p value"]] <- NA
                  ct[["q value"]] <- NA
                }
                ct
                
            })
        })
        
        names(contrast_tables) <- getSelectedContrasts()
        
        contrast_tables
    })
    
    # Filter the contrasts table by the fold change and q value filters
    
    fcMin <- reactive({
        validate(need(input$fcMin, FALSE))
        input$fcMin
    })
    
    qvalMax <- reactive({
        validate(need(input$qvalMax, FALSE))
        input$qvalMax
    })
    
    getFilterRows <- reactive({
        as.logical(input$filterRows)
    })
    
    filteredContrastsTables <- reactive({
      if (getFilterRows()){
        lapply(contrastsTables(), function(ct) ct[abs(ct[["Fold change"]]) >= fcMin() & ct[["q value"]] <= qvalMax(), ])
      }else{
        contrastsTables() 
      }
    })
    
    # Use contrastsTable() to get the data matrix, then apply the appropriate labels. Useful in cases where the matrix is destined for display
    
    labelledContrastsTable <- reactive({
        
        cts <- filteredContrastsTables()
        
        # If we're going to tabulate results from more than one contrast, the tables will need info on the contrasts
        
        if (length(cts) > 1) {
            
            cts <- lapply(names(cts), function(ctn) {
                ct <- cts[[ctn]]
                
                se <- getExperiment()
                contrast <- metadata(se)$contrasts[[as.numeric(ctn)]]
                colnames(ct)[1:2] <- c("Average 1", "Average 2")
                ct$Variable <- prettifyVariablename(contrast[1])
                ct[["Condition 1"]] <- contrast[2]
                ct[["Condition 2"]] <- contrast[3]
                ct[, c("Variable", "Condition 1", "Average 1", "Condition 2", "Average 2", "Fold change", "p value", "q value")]
            })
        }
        
        do.call(rbind, lapply(cts, function(ct) {
            labelMatrix(ct, getExperiment())
        }))
    })
    
    # Use labelledContrastsTable to get the labelled matrix and add some links.
    
    linkedLabelledContrastsTable <- reactive({
        linkMatrix(labelledContrastsTable(), getExperiment())
    })
    
    # Basic accessors for parameters
    
    
    list(fcMin = fcMin, qvalMax = qvalMax, getContrasts = getContrasts, getSelectedContrasts = getSelectedContrasts, getSelectedContrastNames = getSelectedContrastNames, contrastsTables = contrastsTables, 
        filteredContrastsTables = filteredContrastsTables, labelledContrastsTable = labelledContrastsTable, linkedLabelledContrastsTable = linkedLabelledContrastsTable)
}

#' Fold change between two vectors
#'
#' @param vec1 First vector
#' @param vec2 Second vector
#'
#' @return Vector of fold changes
#'
#' @export

foldChange <- function(vec1, vec2) {
    fc <- vec2/vec1
    fc[vec1 == vec2] <- 1
    fc[which(fc < 1)] <- -1/fc[which(fc < 1)]
    fc
} 
