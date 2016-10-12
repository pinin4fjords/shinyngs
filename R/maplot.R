#' The UI input function of the \code{maplot} module
#' 
#' This module produces an MA plot of log(10) expression vs log(2) fold change
#' for contrasts defined in the `contrasts` slot of an 
#' 'ExploratorySummarizedExperimentList` object.
#' 
#' Leverages the \code{contrasts} and \code{scatterplot} modules
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
#' maplotInput('myid', eselist)
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('maplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

maplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    # Only consider experiments that actually have p-values to use in a volcano plot
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    
    # If there's only one experiment, then the expression filters will just be hidden fields, and there's no point in creating an empty fieldset for them
    
    fieldsets <- list()
    if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
        fieldsets$expression_matrix <- expression_filters
    }
    
    fieldsets <- c(fieldsets, list(contrasts = list(contrastsInput(ns("differential"), default_min_foldchange = 1)), scatter_plot = scatterplotInput(ns("ma")), 
        highlight_points = geneselectInput(ns("ma")), export = simpletableInput(ns("matable"))))
    
    inputs <- list(fieldSets(ns("fieldset"), fieldsets))
    
    if (length(eselist) == 1 && length(assays(eselist[[1]])) == 1) {
        inputs <- pushToList(inputs, expression_filters)
    }
    
    inputs
}

#' The output function of the \code{maplot} module
#' 
#' This module produces an MA plot of log(10) expression vs log(2) fold change
#' for contrasts defined in the `contrasts` slot of an 
#' 'ExploratorySummarizedExperimentList` object.
#' 
#' Leverages the \code{contrasts} and \code{scatterplot} modules
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' maplotOutput('experiment')
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('maplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

maplotOutput <- function(id) {
    ns <- NS(id)
    
    list(modalInput(ns("maplot"), "help", "help"), modalOutput(ns("maplot"), "MA plots", includeMarkdown(system.file("inlinehelp", "maplot.md", package = packageName()))), 
        h3("MA plot"), scatterplotOutput(ns("ma")), htmlOutput(ns("matable")))
}

#' The server function of the \code{maplot} module
#' 
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'tests' slot of the ExploratorySummarizedExperiment
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
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
#' callModule(maplot, 'maplot', eselist)
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('maplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

maplot <- function(input, output, session, eselist) {
    
    output$matable <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("matable"), tabletitle = paste("Plot data for contrast", getSelectedContrastNames(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "differential", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = FALSE, getMetafields = getMetafields))
    
    # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight
    
    unpack.list(callModule(geneselect, "ma", eselist = eselist, getExperiment = getExperiment, getAssay = getAssay, provide_all = FALSE, provide_none = TRUE))
    
    # Pass the matrix to the scatterplot module for display
    
    callModule(scatterplot, "ma", getDatamatrix = maTable, getTitle = getSelectedContrastNames, allow_3d = FALSE, getLabels = maLabels, x = 1, y = 2, colorBy = colorBy)
    
    # Extract labels from the volcano table
    
    maLabels <- reactive({
        fct <- maTable()
        fct$label
    })
    
    # Extract a vector use to make colors by group
    
    colorBy <- reactive({
        fct <- maTable()
        fct$colorby
    })
    
    # Make a table of values to use in the volcano plot. Round the values to save space in the JSON
    
    maTable <- reactive({
        
        withProgress(message = "Compiling fold change plot data", value = 0, {
            
            ct <- contrastsTables()[[1]]
            
            matable <- data.frame(`log(10) mean expression` = round(log10(rowMeans(ct[, 1:2])), 3), `log(2) fold change` = round(sign(ct[["Fold change"]]) * log2(abs(ct[["Fold change"]])), 
                3), row.names = rownames(ct), check.names = FALSE)
            
            fct <- filteredContrastsTables()[[1]]
            matable$colorby <- "hidden"
            matable[rownames(fct), "colorby"] <- "match contrast filters"
            matable[selectRows(), "colorby"] <- "in highlighted gene set"
            matable$colorby <- factor(matable$colorby, levels = c("hidden", "match contrast filters", "in highlighted gene set"))
            
            matable$label <- idToLabel(rownames(matable), getExperiment())
            matable$label[!rownames(matable) %in% c(rownames(fct), selectRows())] <- NA
        })
        matable
    })
    
    # Display the data as a table alongside
    
    callModule(simpletable, "matable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, filename = "ma", rownames = FALSE, 
        pageLength = 10)
    
}
