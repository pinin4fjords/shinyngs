#' The UI input function of the selectmarix module
#' 
#' This module uses the geneselect and sampleselect modules to parse controls 
#' defining matrix, row and column and return a matrix.
#' 
#' This will generally not be called directly, but by other modules such as the 
#' heatmap module.
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
#' selectmatrixInput(ns('heatmap'), eselist)

selectmatrixInput <- function(id, eselist, require_tests = FALSE) {
    
    ns <- NS(id)
    
    if (require_tests) {
        eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
            length(ese@tests) > 0
        })))]
    }
    inputs <- list(selectInput(ns("experiment"), "Experiment", names(eselist)), uiOutput(ns("assay")))
    
    # Replace experiment with a hidden input if we've got just the one
    
    if (length(eselist) == 1) {
        inputs[[1]] <- hiddenInput(ns("experiment"), names(eselist)[1])
    }
    
    return(tagList(inputs))
}

#' The server function of the selectmatrix module
#' 
#' This module uses the geneselect and sampleselect modules to parse controls 
#' defining matrix, row and column and return a subsetted matrix.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param var_n The number of rows to select when doing so by variance. Default
#'   = 50
#' @param var_max The maximum umber of rows to select when doing so by variance.
#'   Default = 500
#' @param select_samples Provide UI and functions for sample selection?
#'   (Default: TRUE)
#' @param select_genes Provide UI and functions for gene (row) selection? 
#'   (Default: TRUE)
#' @param provide_all_genes Allow the 'all rows' selection in the UI? Means we 
#'   don't have to calculate variance so the display is quicker, but it's a bad 
#'   idea for e.g. heatmaps where the visual scales by the numbre of rows.
#' @param rounding Number of decimal places to show in results (Default 2)
#'   
#' @return output A list of reactive functions for fetching the derived matrix 
#'   and making a title based on its properties.
#'   
#' @keywords shiny
#'   
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', eselist)

selectmatrix <- function(input, output, session, eselist, var_n = 50, var_max = NULL, select_samples = TRUE, select_genes = TRUE, provide_all_genes = FALSE, 
    require_tests = FALSE, rounding = 2) {
    
    # Use the sampleselect and geneselect modules to generate reactive expressions that can be used to derive an expression matrix
    
    unpack.list(callModule(sampleselect, "selectmatrix", eselist = eselist, getExperiment))
    unpack.list(callModule(geneselect, "selectmatrix", eselist = eselist, getExperiment, var_n = var_n, var_max = varMax(), selectSamples = selectSamples, 
        assay = getAssay, provide_all = provide_all_genes))
    
    # Render controls for selecting the experiment (where a user has supplied multiple SummarizedExpression objects in a list) and assay within each
    
    output$assay <- renderUI({
        withProgress(message = "Rendering assay drop-down", value = 0, {
            validate(need(!is.null(input$experiment), "Waiting for form to provide experiment"))
            
            ns <- session$ns
            
            ese <- getExperiment()
            
            if (length(validAssays()) > 1) {
                assayselect <- selectInput(ns("assay"), "Matrix", validAssays())
            } else {
                assayselect <- hiddenInput(ns("assay"), validAssays()[1])
            }
            list(assayselect, sampleselectInput(ns("selectmatrix"), eselist = eselist, getExperiment = getExperiment, select_samples = select_samples), geneselectInput(ns("selectmatrix"), 
                select_genes = select_genes))
            
        })
    })
    
    # Get list of assays
    
    validAssays <- reactive({
        ese <- getExperiment()
        
        if (require_tests) {
            valid_assays <- names(ese@tests)
        } else {
            names(SummarizedExperiment::assays(ese))
        }
    })
    
    # Reactive for getting the right ExploratorySummarizedExperiment and passing it on to sample and gene selection
    
    getExperiment <- reactive({
        validate(need(input$experiment, FALSE))
        eselist[[input$experiment]]
    })
    
    # Get the row labels where available
    
    getRowLabels <- reactive({
        withProgress(message = "Deriving row labels", value = 0, {
            ese <- getExperiment()
            if (!is.null(ese@idfield)) {
                idToLabel(rownames(ese), ese)
            } else {
                rownames(ese)
            }
        })
    })
    
    # Allow calling modules to retrieve the current assay
    
    getAssay <- reactive({
        input$assay
    })
    
    # Retrieve the assay measure to display with plots etc (where defined by the user)
    
    getAssayMeasure <- reactive({
        ese <- getExperiment()
        if (length(ese@assay_measures) > 0 && getAssay() %in% names(ese@assay_measures)) {
            ese@assay_measures[[getAssay()]]
        } else {
            "expression"
        }
    })
    
    varMax <- reactive({
        if (is.null(var_max)) {
            nrow(getExperiment())
        } else {
            var_max
        }
    })
    
    # Generate an expression matrix given the selected experiment, assay, rows and columns
    
    selectMatrix = reactive({
        withProgress(message = "Getting expression data subset", value = 0, {
            validate(need(!is.null(input$assay), "Waiting for form to provide assay"), need(length(selectSamples()) > 0, "Waiting for sample selection"), 
                need(length(selectRows()) > 0, "No matching rows in selected matrix"))
            
            selected_matrix <- SummarizedExperiment::assays(getExperiment())[[getAssay()]][selectRows(), selectSamples(), drop = FALSE]
            selected_matrix <- selected_matrix[complete.cases(selected_matrix), ]
            
            if (getSampleSelect() == "group" && getSummaryType() != "none") {
                selected_matrix <- summarizeMatrix(selected_matrix, data.frame(selectColData())[[getSampleGroupVar()]], getSummaryType())
            }
            apply(selected_matrix, 2, round, rounding)
        })
    })
    
    # Extract experimental variables given selection parameters
    
    selectColData = reactive({
        validate(need(length(selectSamples()) > 0, "Waiting for sample selection"))
        withProgress(message = "Extracting experiment metadata", value = 0, {
            droplevels(data.frame(colData(getExperiment())[selectSamples(), , drop = FALSE]))
        })
    })
    
    # Calling modules may need to know if the data are sumamrised. E.g. heatmaps only need to display sample metadata for unsummarised matrices Will only be
    # summarised if grouping variables were supplied!
    
    isSummarised <- reactive({
        length(eselist@group_vars) > 0 && getSummaryType() != "none"
    })
    
    # Extract the annotation from the SummarizedExperiment
    
    getAnnotation = reactive({
        data.frame(mcols(getExperiment()))
    })
    
    # Use selectMatrix() to get the data matrix, then apply the appropriate labels. Useful in cases where the matrix is destined for display
    
    selectLabelledMatrix <- reactive({
        
        selected_matrix <- data.frame(selectMatrix())
        se <- getExperiment()
        
        labelMatrix(selected_matrix, se)
    })
    
    # Use selectLabelledMatrix to get the labelled matrix and add some links.
    
    selectLabelledLinkedMatrix <- reactive({
        selected_matrix <- selectLabelledMatrix()
        se <- getExperiment()
        
        if (length(eselist@url_roots) > 0) {
            linkMatrix(selected_matrix, eselist@url_roots)
        } else {
            selected_matrix
        }
    })
    
    # Return the list of reactive expressions we'll need to access the data
    
    list(getExperiment = getExperiment, getAssayMeasure = getAssayMeasure, selectMatrix = selectMatrix, selectLabelledMatrix = selectLabelledMatrix, matrixTitle = title, 
        selectColData = selectColData, isSummarised = isSummarised, getAssay = getAssay, selectLabelledLinkedMatrix = selectLabelledLinkedMatrix, getRowLabels = getRowLabels)
}

#' Add columns to display ID and label in a table
#'
#' Labels only added if \code{labelfield} is specified in \code{se}
#'
#' @param matrix The input table
#' @param ese An ExploratorySummarizedExperiment object
#'
#' @return output Table with columns added

labelMatrix <- function(matrix, ese) {
    
    withProgress(message = "Adding labels to matrix", value = 0, {
        
        datacolnames <- colnames(matrix)
        
        idfield <- ese@idfield
        matrix[[idfield]] <- rownames(matrix)
        
        if (length(ese@labelfield) > 0) {
            annotation <- data.frame(mcols(ese))
            labelfield <- ese@labelfield
            
            matrix[[labelfield]] <- annotation[match(rownames(matrix), annotation[[idfield]]), labelfield]
            matrix <- matrix[, c(idfield, labelfield, datacolnames)]
            
            colnames(matrix)[colnames(matrix) == labelfield] <- prettifyVariablename(labelfield)
        } else {
            matrix <- matrix[, c(idfield, datacolnames)]
        }
        
        # Make the field identifiers nicer
        
        colnames(matrix)[colnames(matrix) == idfield] <- prettifyVariablename(idfield)
    })
    matrix
}

#' Add links to a table
#'
#' Root URLs must be present in the \code{url_roots} slot of \code{se}
#'
#' @param matrix The input table
#' @param url_roots A list with URL roots, with names matching columns of 
#' \code{matrix}
#' @param display_values A matrix which may contain values for displaying 
#' in the link, differerent from that used in the href.
#'
#' @return output Table with links added

linkMatrix <- function(matrix, url_roots, display_values = data.frame()) {
    
    # Add prettified version of each field in URL roots in case matrix column names are prettified
    
    for (fieldname in names(url_roots)) {
        url_roots[[prettifyVariablename(fieldname)]] <- url_roots[[fieldname]]
    }
    
    for (fieldname in names(url_roots)) {
        if (fieldname %in% colnames(matrix)) {
            
            matrix[[fieldname]] <- unlist(lapply(1:length(matrix[[fieldname]]), function(x) {
                fvs_for_href <- unlist(strsplit(matrix[[fieldname]][x], " "))
                
                # If user has specified different values for display, use them
                
                if (fieldname %in% colnames(display_values)) {
                  fvs_for_display <- display_values[x, fieldname]
                } else {
                  fvs_for_display <- fvs_for_href
                }
                
                paste(paste0("<a href='", url_roots[fieldname], fvs_for_href, "'>", fvs_for_display, "</a>"), collapse = " ")
            }))
        }
    }
    matrix
}

#' Create row labels based on the settings of \code{labelfield} in the 
#' SummarizedExperiment object and the annotation data in \code{mcols}
#'
#' @param list of ids
#' @param ese An ExploratorySummarizedExperiment
#'
#' @return String vector of same length as \code{ids}
#' @export

idToLabel <- function(ids, ese) {
    if (length(ese@labelfield) == 0) {
        ids
    } else {
        labels <- convertIds(ids, ese, ese@labelfield)
        labels[!is.na(labels)] <- paste(labels[!is.na(labels)], ids[!is.na(labels)], sep = " / ")
        labels[is.na(labels)] <- ids[is.na(labels)]
        labels
    }
}

#' Convert row names to metadata identifiers
#'
#' @param ids IDs found as row names in the 
#' \code{ExploratorySummarizedExperiment}
#' @param ese The \code{ExploratorySummarizedExperiment}
#' @param to The metadata column (via \code{mcols}) to use
#' @param remove_na Take out NAs? Not done by default to preserve vector length
#'
#' @return output Vector of converted ids
#' @export

convertIds <- function(ids, ese, to, remove_na = FALSE) {
    annotation <- data.frame(mcols(ese))
    
    converted <- annotation[match(ids, annotation[[ese@idfield]]), to]
    if (remove_na) {
        converted <- converted[!is.na(converted)]
    }
    converted
} 
