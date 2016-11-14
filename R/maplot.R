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
    
    fieldsets <- c(fieldsets, list(contrasts = list(contrastsInput(ns("differential"))), scatter_plot = scatterplotInput(ns("ma")), highlight_points = geneselectInput(ns("ma")), 
        export = simpletableInput(ns("matable"))))
    
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
        
        simpletableOutput(ns("matable"), tabletitle = paste("Plot data for contrast", getSelectedContrastNames()[[1]], sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    selectmatrix_reactives <- callModule(selectmatrix, "expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE)
    unpack.list(selectmatrix_reactives)
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE))
    
    # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight
    
    unpack.list(callModule(geneselect, "ma", eselist = eselist, getExperiment = getExperiment, getAssay = getAssay, provide_all = FALSE, provide_none = TRUE))
    
    # Pass the matrix to the scatterplot module for display
    
    callModule(scatterplot, "ma", getDatamatrix = maTable, getTitle = getTitle, allow_3d = FALSE, getLabels = maLabels, x = 1, y = 2, colorBy = colorBy, getLines = plotLines)
    
    
    # Make a title by selecting the single contrast name of the single filter set
    
    getTitle <- reactive({
        contrast_names <- getSelectedContrastNames()
        contrast_names[[1]][[1]]
    })
    
    # Make a set of dashed lines to overlay on the plot representing thresholds
    
    plotLines <- reactive({
        withProgress(message = "Calculating lines", value = 0, {
            
            mat <- maTable()
            
            fclim <- getFoldChange()
            
            normal_y <- !is.infinite(mat[, 2])
            normal_x <- !is.infinite(mat[, 1])
            
            ymax <- max(mat[normal_y, 2], na.rm = TRUE)
            ymin <- min(mat[normal_y, 2], na.rm = TRUE)
            
            xmax <- max(mat[normal_x, 1], na.rm = TRUE)
            xmin <- min(mat[normal_x, 1], na.rm = TRUE)
            
            lines <- data.frame(name = c(rep(paste0(abs(fclim), "-fold down"), 2), rep(paste0(abs(fclim), "-fold up"), 2)), x = c(xmin, xmax, xmin, xmax), y = c(rep(-log2(abs(fclim)), 
                2), rep(log2(abs(fclim)), 2)))
            
            # Use lines dependent on how the fold change filter is applied
            
            fccard <- getFoldChangeCard()
            if (fccard %in% c("> or <-", "< and >-")) {
                lines
            } else if (fccard == "<" && sign(fclim) == "-1") {
                droplevels(lines[c(1, 2), ])
            } else {
                droplevels(lines[c(3, 4), ])
            }
            
        })
        
    })
    
    
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
            
            sct <- selectedContrastsTables()
            ct <- sct[[1]][[1]]
            
            matable <- data.frame(`log(10) mean expression` = round(log10(rowMeans(ct[, 1:2])), 3), `log(2) fold change` = round(sign(ct[["Fold change"]]) * log2(abs(ct[["Fold change"]])), 
                3), row.names = rownames(ct), check.names = FALSE)
            
            fct <- filteredContrastsTables()[[1]][[1]]
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
    
    callModule(simpletable, "matable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, filename = "ma", rownames = FALSE, pageLength = 10)
    
} 
