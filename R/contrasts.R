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
#' @param summarise Provide summarisation controls? Allow user to control how 
#'   how values are summarised per group. Disabling this disables 
#'   summarisation, which may be the desired result for modules that just need
#'   to use the contrasts drop-down. 
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' contrastsInput('test')

contrastsInput <- function(id, default_min_foldchange = 2, allow_filtering = TRUE, summarise = TRUE) {
    
    ns <- NS(id)
    
    inputs <- list(uiOutput(ns("contrasts")))
    
    if (allow_filtering) {
        
        inputs <- pushToList(inputs, checkboxInput(ns("filterRows"), "Filter rows", TRUE))
        inputs <- pushToList(inputs, conditionalPanel(condition = paste0("input['", ns("filterRows"), "'] == true"), numericInput(ns("fcMin"), 
            "Minimum absolute fold change", value = default_min_foldchange), uiOutput(ns("pvalMax")), uiOutput(ns("qvalMax"))))
    } else {
        inputs <- pushToList(inputs, shinyjs::hidden(checkboxInput(ns("filterRows"), "Filter rows", FALSE)))
    }
    
    if (summarise) {
        inputs <- pushToList(inputs, summarisematrixInput(ns("contrasts"), allow_none = FALSE))
    }
    inputs
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
#' @param selectMatrix Reactive for generating a matrix to do comparisons with.
#' Probably returned by a call to the \code{\link{selectmatrix}} module.
#' @param selectColData Reactive returning column metadata for the matrix 
#' provided by selectMatrix. Probably returned by a call to the 
#' \code{\link{selectmatrix}} module.
#' @param getAssay Reactive for fetching the current assay. 
#' @param multiple Allow selection of multiple contrasts?
#' @param show_controls Show the controls for contrast selection? 
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(contrasts, 'differential', getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE)

contrasts <- function(input, output, session, eselist, getExperiment = NULL, selectMatrix = NULL, selectColData = NULL, 
    getAssay = NULL, getMetafields = NULL, multiple = FALSE, show_controls = TRUE) {
    
    ns <- session$ns
    
    getSummaryType <- callModule(summarisematrix, "contrasts")
    
    # Render the controls depending on currently selected experiment etc.
    
    output$contrasts <- renderUI({
        
        contrast_numbers <- getAllContrastsNumbers()
        
        summaries <- getSummaries()
        contrasts <- getAllContrasts()
        
        contrast_numbers <- contrast_numbers[unlist(lapply(contrasts, function(x) all(x[-1] %in% colnames(summaries[[x[1]]]))))]
        
        if (!is.null(contrast_numbers)) {
            
            if (multiple) {
                cont_control <- checkboxGroupInput(ns("contrasts"), "Contrast(s):", contrast_numbers, selected = contrast_numbers)
            } else {
                cont_control <- selectInput(ns("contrasts"), "Contrast(s):", contrast_numbers)
            }
            
            if (!show_controls) {
                cont_control <- shinyjs::hidden(cont_control)
            }
            cont_control
        }
    })
    
    # Only some assays have associated stats
    
    output$qvalMax <- renderUI({
        ese <- getExperiment()
        assay <- getAssay()
        
        if ("qvals" %in% names(ese@tests[[assay]]) && !is.null(ese@tests[[assay]]$qvals)) {
            numericInput(ns("qvalMax"), "Maximum q value", value = 0.1)
        } else {
            hiddenInput(ns("qvalMax"), 1)
        }
    })
    
    output$pvalMax <- renderUI({
        ese <- getExperiment()
        assay <- getAssay()
        
        if ("pvals" %in% names(ese@tests[[assay]]) && !is.null(ese@tests[[assay]]$pvals)) {
            numericInput(ns("pvalMax"), "Maximum p value", value = 0.05)
        } else {
            hiddenInput(ns("pvalMax"), 1)
        }
    })
    
    # Get all the contrasts the user specified in their StructuredExperiment- if any
    
    getAllContrasts <- reactive({
        if (length(eselist@contrasts) > 0) {
            eselist@contrasts
        } else {
            NULL
        }
    })
    
    # Get a named vector of integers for contrasts, to be used in field etc
    
    getAllContrastsNumbers <- reactive({
        contrasts <- getAllContrasts()
        contrast_names <- makeContrastNames()
        
        if (!is.null(contrasts)) {
            structure(1:length(contrasts), names = contrast_names)
        } else {
            NULL
        }
    })
    
    # Make names for the contrasts
    
    makeContrastNames <- reactive({
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(x) paste(prettifyVariablename(x[1]), paste(x[3], x[2], sep = " vs "), sep = ": "))
    })
    
    makeSafeContrastNames <- reactive({
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(x) paste(ucfirst(prettifyVariablename(x[1])), paste(ucfirst(x[3]), ucfirst(x[2]), sep = "VS"), 
            sep = "."))
    })
    
    # Get the index of the currently selected contrast
    
    getSelectedContrastNumbers <- reactive({
        validate(need(!is.null(input$contrasts), "Waiting for contrasts"))
        as.numeric(input$contrasts)
    })
    
    # Get the actual contrasts to which the numbers from the interface pertain
    
    getSelectedContrasts <- reactive({
        eselist@contrasts[getSelectedContrastNumbers()]
    })
    
    # Get the name of the currently selected contrast
    
    getSelectedContrastNames <- reactive({
        contrast_names <- makeContrastNames()
        contrast_names[getSelectedContrastNumbers()]
    })
    
    # The same, but with safe names that won't get mangled by plotting etc
    
    getSafeSelectedContrastNames <- reactive({
        contrast_names <- makeSafeContrastNames()
        contrast_names[getSelectedContrastNumbers()]
    })
    
    # Get list describing, for each contrast, the samples on each side
    
    getContrastSamples <- reactive({
        ese <- getExperiment()
        coldata <- selectColData()
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(c) {
            list(colnames(ese)[coldata[c[1]] == c[2]], colnames(ese)[coldata[c[1]] == c[3]])
        })
    })
    
    # Get samples for currently selected contrast
    
    getSelectedContrastSamples <- reactive({
        contrast_samples <- getContrastSamples()
        selected_contrasts <- getSelectedContrastNumbers()
        contrast_samples[[selected_contrasts]]
    })
    
    # Generate the summary statistic (probably mean) for column groups as defined by the possible contrasts. Other
    # functions can then pick from this output and calculate fold changes etc.
    
    getSummaries <- reactive({
        ese <- getExperiment()
        contrasts <- getAllContrasts()
        matrix <- selectMatrix()
        coldata <- selectColData()
        
        validate(need(nrow(matrix) > 0, "Waiting for input matrix"))
        
        contrast_variables <- unique(unlist(lapply(contrasts, function(x) x[1])))
        names(contrast_variables) <- contrast_variables
        
        withProgress(message = paste("Calculating summaries by", getSummaryType()), value = 0, {
            summaries <- lapply(contrast_variables, function(cv) summarizeMatrix(matrix, coldata[[cv]], getSummaryType()))
        })
        
        summaries
    })
    
    # Filter the contrasts table by the fold change and q value filters
    
    fcMin <- reactive({
        validate(need(input$fcMin, FALSE))
        input$fcMin
    })
    
    # Get current value of the q value filter
    
    qvalMax <- reactive({
        validate(need(input$qvalMax, FALSE))
        input$qvalMax
    })
    
    # Get current value of the p value filter
    
    pvalMax <- reactive({
        validate(need(input$pvalMax, FALSE))
        input$pvalMax
    })
    
    # Get current value of field which determines if the table should be filtered at all.
    
    getFilterRows <- reactive({
        as.logical(input$filterRows)
    })
    
    # Main function for returning the table of contrast information. Means, fold changes calculated on the fly, p/q values
    # must be supplied in a 'tests' slot of the ExploratorySummarizedExperiment.
    
    contrastsTables <- reactive({
        matrix <- selectMatrix()
        
        ese <- getExperiment()
        summaries <- getSummaries()
        contrasts <- getAllContrasts()
        selected_contrasts <- getSelectedContrasts()
        
        # There can be a mismatch between the conrasts and summaries as we adjust the input matrix. Wait for updates to finish
        # before making the table.
        
        validate(need(all(unlist(lapply(selected_contrasts, function(x) all(x[-1] %in% colnames(summaries[[x[1]]]))))), 
            "Matching summaries and contrasts"))
        
        withProgress(message = "Calculating summary data", value = 0, {
            
            contrast_tables <- lapply(getSelectedContrastNumbers(), function(c) {
                
                cont <- contrasts[[c]]
                
                smry1 <- summaries[[cont[1]]][, cont[2], drop = FALSE]
                smry2 <- summaries[[cont[1]]][, cont[3], drop = FALSE]
                
                ct <- data.frame(round(smry1, 2), round(smry2, 2), round(foldChange(smry1, smry2), 2))
                names(ct) <- c(cont[2], cont[3], "Fold change")
                
                if (length(ese@tests) > 0 && getAssay() %in% names(ese@tests)) {
                  pvals <- ese@tests[[getAssay()]]$pvals
                  qvals <- ese@tests[[getAssay()]]$qvals
                  
                  ct[["p value"]] <- signif(pvals[match(rownames(ct), rownames(pvals)), c], 5)
                  ct[["q value"]] <- signif(qvals[match(rownames(ct), rownames(qvals)), c], 5)
                  
                } else {
                  ct[["p value"]] <- NA
                  ct[["q value"]] <- NA
                }
                ct
                
            })
        })
        
        names(contrast_tables) <- getSelectedContrastNumbers()
        contrast_tables
    })
    
    # Apply user filters to results of contrastsTables()
    
    filteredContrastsTables <- reactive({
        ese <- getExperiment()
        
        if (getFilterRows()) {
            if (length(ese@tests) == 0 || !getAssay() %in% names(ese@tests)) {
                lapply(contrastsTables(), function(ct) ct[abs(ct[["Fold change"]]) >= fcMin(), , drop = FALSE])
            } else {
                lapply(contrastsTables(), function(ct) ct[abs(ct[["Fold change"]]) >= fcMin() & ct[["p value"]] <= pvalMax() & 
                  ct[["q value"]] <= qvalMax(), , drop = FALSE])
            }
        } else {
            contrastsTables()
        }
    })
    
    # Use contrastsTable() to get the data matrix, filter with filteredContrastsTables()then apply the appropriate labels.
    # Useful in cases where the matrix is destined for display.
    
    labelledContrastsTable <- reactive({
        
        cts <- filteredContrastsTables()

        # If we're going to tabulate results from more than one contrast, the tables will need info on the contrasts
        
        if (length(cts) > 1) {
            
            cts <- lapply(names(cts), function(ctn) {
                ct <- cts[[ctn]]
                
                ese <- getExperiment()
                contrast <- eselist@contrasts[[as.numeric(ctn)]]
                colnames(ct)[1:2] <- c("Average 1", "Average 2")
                ct$Variable <- prettifyVariablename(contrast[1])
                ct[["Condition 1"]] <- contrast[2]
                ct[["Condition 2"]] <- contrast[3]
                ct[, c("Variable", "Condition 1", "Average 1", "Condition 2", "Average 2", "Fold change", "p value", "q value"), 
                  drop = FALSE]
            })
        }
        
        labelled_contrasts_table <- do.call(rbind, lapply(cts, function(ct) {
            metafields <- c()
            if (!is.null(getMetafields)) {
                metafields <- getMetafields()
            }
            labelMatrix(ct, getExperiment(), metafields = metafields)
        }))
        
        validate(need(nrow(labelled_contrasts_table) > 0, "No results matching specified filters"))
        
        labelled_contrasts_table
    })
    
    # Use labelledContrastsTable to get the labelled matrix and add some links.
    
    linkedLabelledContrastsTable <- reactive({
        if (length(eselist@url_roots) > 0) {
            linkMatrix(labelledContrastsTable(), eselist@url_roots)
        } else {
            labelledContrastsTable()
        }
    })
    
    # A summary table of differential expression
    
    makeDifferentialSetSummary <- reactive({
        fcts <- filteredContrastsTables()
        contrasts <- getSelectedContrasts()
        
        summary <- data.frame(do.call(rbind, contrasts))
        colnames(summary) <- c("Variable", "group 1", "group 2")
        summary[["Set size"]] <- unlist(lapply(fcts, nrow))
        summary
    })
    
    # Basic accessors for parameters
    
    list(fcMin = fcMin, qvalMax = qvalMax, getAllContrasts = getAllContrasts, getSelectedContrasts = getSelectedContrasts, 
        getSelectedContrastNumbers = getSelectedContrastNumbers, getSelectedContrastNames = getSelectedContrastNames, getSafeSelectedContrastNames = getSafeSelectedContrastNames, 
        getContrastSamples = getContrastSamples, getSelectedContrastSamples = getSelectedContrastSamples, contrastsTables = contrastsTables, 
        filteredContrastsTables = filteredContrastsTables, labelledContrastsTable = labelledContrastsTable, linkedLabelledContrastsTable = linkedLabelledContrastsTable, 
        makeDifferentialSetSummary = makeDifferentialSetSummary)
}

#' Calculate fold change between two vectors
#'
#' @param vec1 First vector
#' @param vec2 Second vector
#'
#' @return Vector of fold changes
#'
#' @export
#' @examples 
#' foldChange(c(2,2,2), c(2,4,8))

foldChange <- function(vec1, vec2) {
    fc <- vec2/vec1
    fc[vec1 == vec2] <- 1
    fc[which(fc < 1)] <- -1/fc[which(fc < 1)]
    fc
} 
