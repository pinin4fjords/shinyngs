#' The UI input function of the volcanoplot module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'tests' slot of the ExploratorySummarizedExperiment
#' 
#' Leverages the \code{scatterplot} module
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
#' differentialtableInput('experiment', eselist)

volcanoplotInput <- function(id, eselist) {
    ns <- NS(id)
    
    # Only consider experiments that actually have p-values to use in a volcano plot
    
    eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
        length(ese@tests) > 0
    })))]
    expression_filters <- selectmatrixInput(ns("expression"), eselist, require_tests = TRUE)
    
    # If there's only one experiment with tests, then the expression filters will just be hidden fields, and there's no point in creating an empty fieldset
    # for them
    
    fieldsets <- list()
    if (length(eselist) > 1) {
        fieldsets$expression_matrix <- expression_filters
    }
    
    fieldsets <- c(fieldsets, list(contrasts = list(contrastsInput(ns("differential"))), scatter_plot = scatterplotInput(ns("volcano")), highlight_points = geneselectInput(ns("volcano")), 
        export = simpletableInput(ns("differentialtable"))))
    
    inputs <- list(fieldSets(ns("fieldset"), fieldsets))
    
    if (length(eselist) == 1) {
        inputs <- pushToList(inputs, expression_filters)
    }
    
    inputs
}

#' The output function of the differentialtable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'tests' slot of the ExploratorySummarizedExperiment
#' 
#' Leverages the \code{scatterplot} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' differentialtableOutput('experiment')

volcanoplotOutput <- function(id) {
    ns <- NS(id)
    
    list(h3('Volcano plot'), scatterplotOutput(ns("volcano")), htmlOutput(ns("volcanotable")))
}

#' The server function of the differentialtable module
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
#' callModule(differentialtable, 'differentialtable', eselist)

volcanoplot <- function(input, output, session, eselist) {
    
    output$volcanotable <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("volcanotable"), tabletitle = paste("Plot data for contrast", getSelectedContrastNames(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE, require_tests = TRUE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "differential", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, 
        multiple = FALSE))
    
    # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight
    
    unpack.list(callModule(geneselect, "volcano", eselist = eselist, getExperiment = getExperiment, assay = getAssay, provide_all = FALSE, provide_none = TRUE))
    
    # Pass the matrix to the scatterplot module for display
    
    callModule(scatterplot, "volcano", getDatamatrix = volcanoTable, title = getSelectedContrastNames(), 
        allow_3d = FALSE, getLabels = volcanoLabels, x = 1, y = 2, colorby = colorby, getLines = plotLines)
    
    # Make a set of dashed lines to overlay on the plot representing thresholds
    
    plotLines <- reactive({
        withProgress(message = "Calculating lines", value = 0, {
            
            vt <- volcanoTable()
            
            fclim <- log2(fcMin())
            qvallim <- -log10(qvalMax())
            
            normal_y <- !is.infinite(vt[, 2])
            normal_x <- !is.infinite(vt[, 1])
            
            ymax <- max(vt[normal_y, 2])
            ymin <- min(vt[normal_y, 2])
            
            xmax <- max(vt[normal_x, 1])
            xmin <- min(vt[normal_x, 1])
            
            data.frame(name = c(rep("xmin", 2), rep("xmax", 2), rep("ymin", 2)), x = c(rep(-fclim, 2), rep(fclim, 2), xmin, xmax), y = c(ymin, ymax, ymin, 
                ymax, rep(qvallim, 2)))
        })
        
    })
    
    # Extract labels from the volcano table
    
    volcanoLabels <- reactive({
        withProgress(message = "Making labels", value = 0, {
            vt <- volcanoTable()
            vt$label
        })
    })
    
    # Extract a vector use to make colors by group
    
    colorby <- reactive({
        vt <- volcanoTable()
        vt$colorby
    })
    
    # Make a table of values to use in the volcano plot. Round the values to save space in the JSON
    
    volcanoTable <- reactive({
        withProgress(message = "Compiling volcano plot data", value = 0, {
            
            ct <- contrastsTables()[[1]]
            ct <- ct[, c("Fold change", "q value")]
            ct[["Fold change"]] <- round(sign(ct[["Fold change"]]) * log2(abs(ct[["Fold change"]])), 3)
            ct[["q value"]] <- round(-log10(ct[["q value"]]), 3)
            
            cont <- getContrasts()[[1]]
            colnames(ct) <- c(paste(paste0("(higher in ", cont[2], ")"), "log2(fold change)", paste0("(higher in ", cont[3], ")"), sep = "  "), "-log10(q value)")
            
            fct <- filteredContrastsTables()[[1]]
            ct$colorby <- "hidden"
            ct[rownames(fct), "colorby"] <- "match contrast filters"
            ct[selectRows(), "colorby"] <- "in highlighted gene set"
            
            ct$label <- idToLabel(rownames(ct), getExperiment())
            ct$label[!rownames(ct) %in% c(rownames(fct), selectRows())] <- NA
        })
        ct
    })
    
    # Display the data as a table alongside
    
    callModule(simpletable, "volcanotable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, filename = "volcano", rownames = FALSE, 
        pageLength = 10)
    
} 
