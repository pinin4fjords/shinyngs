#' The UI input function of the \code{foldchangeplot} module
#' 
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperimentList
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
#' data(zhangneurons)
#' foldchangeplotInput('myid', zhangneurons)
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('foldchangeplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

foldchangeplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    # Only consider experiments that actually have p-values to use in a volcano plot
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    
    # If there's only one experiment, then the expression filters will just be hidden fields, and there's no point in creating an
    # empty fieldset for them
    
    fieldsets <- list()
    if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
        fieldsets$expression_matrix <- expression_filters
    }
    
    fieldsets <- c(fieldsets, list(contrasts = list(contrastsInput(ns("differential"))), scatter_plot = scatterplotInput(ns("foldchange")), 
        highlight_points = geneselectInput(ns("foldchange")), export = simpletableInput(ns("foldchangetable"))))
    
    inputs <- list(fieldSets(ns("fieldset"), fieldsets))
    
    if (length(eselist) == 1 && length(assays(eselist[[1]])) == 1) {
        inputs <- pushToList(inputs, expression_filters)
    }
    
    inputs
}

#' The output function of the \code{foldchangeplot} module
#' 
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperimentList
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
#' foldchangeplotOutput('myid')
#' 
#' data(zhangneurons)
#' app <- prepareApp('foldchangeplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

foldchangeplotOutput <- function(id) {
    ns <- NS(id)
    
    list(modalInput(ns("foldchangeplot"), "help", "help"), modalOutput(ns("foldchangeplot"), "Fold change plots", includeMarkdown(system.file("inlinehelp", 
        "foldchangeplot.md", package = packageName()))), h3("Fold change plot"), scatterplotOutput(ns("foldchange")), htmlOutput(ns("foldchangetable")))
}

#' The server function of the \code{foldchangeplot} module
#' 
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperimentList
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
#' callModule(foldchangeplot, 'foldchangeplot', eselist)
#' 
#' data(zhangneurons)
#' app <- prepareApp('foldchangeplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

foldchangeplot <- function(input, output, session, eselist) {
    
    output$foldchangetable <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("foldchangetable"), tabletitle = paste("Plot data for contrast", getSelectedContrastNames(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    selectmatrix_reactives <- callModule(selectmatrix, "expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE)
    unpack.list(selectmatrix_reactives)
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "differential", eselist = eselist, multiple = FALSE, selectmatrix_reactives = selectmatrix_reactives))
    
    # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight
    
    unpack.list(callModule(geneselect, "foldchange", eselist = eselist, getExperiment = getExperiment, getAssay = getAssay, provide_all = FALSE, 
        provide_none = TRUE))
    
    # Pass the matrix to the scatterplot module for display
    
    callModule(scatterplot, "foldchange", getDatamatrix = foldchangeTable, getTitle = getSelectedContrastNames[[1]], allow_3d = FALSE, 
        getLabels = foldchangeLabels, x = 1, y = 2, colorBy = colorBy, getLines = plotLines)
    
    # Make a set of dashed lines to overlay on the plot representing thresholds
    
    plotLines <- reactive({
        
        fct <- foldchangeTable()
        
        fclim <- log2(fcMin())
        
        normal_y <- !is.infinite(fct[, 2])
        normal_x <- !is.infinite(fct[, 1])
        
        ymax <- max(fct[normal_y, 2])
        ymin <- min(fct[normal_y, 2])
        
        xmax <- max(fct[normal_x, 1])
        xmin <- min(fct[normal_x, 1])
        
        min <- min(xmin, ymin)
        max <- max(xmax, ymax)
        
        lines <- data.frame(name = c(rep("No change", 2), rep(paste0(fcMin(), "-fold down"), 2), rep(paste0(fcMin(), "-fold up"), 
            2)), x = c(min, max, min, max, min, max), y = c(c(min, max), (min - log2(fcMin())), (max - log2(fcMin())), (min + 
            log2(fcMin())), (max + log2(fcMin()))), stringsAsFactors = FALSE)
        lines$name <- factor(lines$name, levels = unique(lines$name))
        
        lines
        
    })
    
    # Extract labels from the volcano table
    
    foldchangeLabels <- reactive({
        fct <- foldchangeTable()
        fct$label
    })
    
    # Extract a vector use to make colors by group
    
    colorBy <- reactive({
        fct <- foldchangeTable()
        fct$colorby
    })
    
    # Make a table of values to use in the volcano plot. Round the values to save space in the JSON
    
    foldchangeTable <- reactive({
        
        withProgress(message = "Compiling fold change plot data", value = 0, {
            sct <- selectedContrastsTables()
            saveRDS(sct, file = "/tmp/sct.rds")
            ct <- selectedContrastsTables()[[1]][[1]]
            ct <- round(log2(ct[, 1:2]), 3)
            
            cont <- getSelectedContrasts()[[1]][[1]]
            colnames(ct) <- c(paste0("log2(", cont[2], ")"), paste0("log2(", cont[3], ")"))
            
            fct <- filteredContrastsTables()[[1]][[1]]
            ct$colorby <- "hidden"
            ct[rownames(fct), "colorby"] <- "match contrast filters"
            ct[selectRows(), "colorby"] <- "in highlighted gene set"
            ct$colorby <- factor(ct$colorby, levels = c("hidden", "match contrast filters", "in highlighted gene set"))
            
            ct$label <- idToLabel(rownames(ct), getExperiment())
            ct$label[!rownames(ct) %in% c(rownames(fct), selectRows())] <- NA
        })
        ct
    })
    
    # Display the data as a table alongside
    
    callModule(simpletable, "foldchangetable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, 
        filename = "foldchange", rownames = FALSE, pageLength = 10)
    
} 
