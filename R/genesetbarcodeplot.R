#' The UI input function of the genesetbarcodeplot module
#' 
#' This module leverages gene sets stored in the \code{gene_sets} slot of an
#' \code{ExploratorySummarizedExperimentList} object to produce barcode plots
#' using Limma's \code{\link[limma]{barcodeplot}} function. Genes are ranked by
#' fold changes calculated with the \code{contrasts} module, and FDR values 
#' from the \code{gene_set_analyses} slot of the selected 
#' \code{ExploratorySummarizedExperiment} are displayed where provided.
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
#' genesetbarcodeplotInput('myid', zhangneurons)
#' 
#' # Almost certainly used via application creation
#' 
#' app <- prepareApp('genesetbarcodeplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

genesetbarcodeplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    # Only use experiments with gene set analyses available
    
    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]
    
    # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay
    # select doesn't have invalid options.
    
    for (exp in names(eselist)) {
        assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
    }
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    
    field_sets <- list(gene_set = genesetselectInput(ns("genesetbarcodeplot"), multiple = FALSE), contrast = contrastsInput(ns("genesetbarcodeplot"), 
        allow_filtering = FALSE))
    
    # Things we don't want to wrap in a field set - probably hidden stuff
    
    naked_fields = list()
    
    if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
        field_sets$select_assay_data <- expression_filters
    } else {
        naked_fields <- pushToList(naked_fields, expression_filters)
    }
    
    field_sets <- c(field_sets, list(export = list(p(simpletableInput(ns("genesetbarcodeplot"), "Gene set")), plotdownloadInput(ns("genesetbarcodeplot")))))
    
    list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The output function of the genesetbarcodeplot module
#' 
#' This module leverages gene sets stored in the \code{gene_sets} slot of an
#' \code{ExploratorySummarizedExperimentList} object to produce barcode plots
#' using Limma's \code{\link[limma]{barcodeplot}} function. Genes are ranked by
#' fold changes calculated with the \code{contrasts} module, and FDR values 
#' from the \code{gene_set_analyses} slot of the selected 
#' \code{ExploratorySummarizedExperiment} are displayed where provided.
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' genesetbarcodeplotOutput('experiment')
#' 
#' # Almost certainly used via application creation
#' 
#' app <- prepareApp('genesetbarcodeplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

genesetbarcodeplotOutput <- function(id) {
    ns <- NS(id)
    
    list(modalInput(ns("genesetbarcodeplot"), "help", "help"), modalOutput(ns("genesetbarcodeplot"), "Gene set barcode plot", 
        includeMarkdown(system.file("inlinehelp", "genesetbarcodeplot.md", package = packageName()))), h3("Gene set barcode plot"), 
        plotOutput(ns("genesetbarcodeplot")), h4("Gene set differential expression"), simpletableOutput(ns("genesetbarcodeplot")))
}

#' The server function of the genesetbarcodeplot module
#' 
#' This module leverages gene sets stored in the \code{gene_sets} slot of an
#' \code{ExploratorySummarizedExperimentList} object to produce barcode plots
#' using Limma's \code{\link[limma]{barcodeplot}} function. Genes are ranked by
#' fold changes calculated with the \code{contrasts} module, and FDR values 
#' from the \code{gene_set_analyses} slot of the selected 
#' \code{ExploratorySummarizedExperiment} are displayed where provided.
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
#' callModule(genesetbarcodeplot, 'genesetbarcodeplot', eselist)
#' 
#' # Almost certainly used via application creation
#' 
#' app <- prepareApp('genesetbarcodeplot', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

genesetbarcodeplot <- function(input, output, session, eselist) {
    
    # Only use experiments with gene set analyses available
    
    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]
    
    # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay
    # select doesn't have invalid options.
    
    for (exp in names(eselist)) {
        assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
    }
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, select_samples = FALSE, select_genes = FALSE, select_meta = FALSE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "genesetbarcodeplot", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, 
        getAssay = getAssay, multiple = FALSE, getMetafields = getMetafields))
    
    # Parse the gene sets for ease of use
    
    unpack.list(callModule(genesetselect, "genesetbarcodeplot", eselist, getExperiment, multiple = FALSE))
    
    # Call to plotdownload module
    
    callModule(plotdownload, "genesetbarcodeplot", makePlot = plotGenesetBarcodeplot, filename = "genesetbarcodeplot.png", 
        plotHeight = 600, plotWidth = 800)
    
    observe({
        updateGeneSetsList()
    })
    
    # Make a sensible title for the plot
    
    barcodeplotTitle <- reactive({
        ese <- getExperiment()
        
        title_components <- c(prettifyGeneSetName(unlist(getGenesetNames())), getSelectedContrastNames())
        
        gene_set_types <- getGenesetTypes()
        assay <- getAssay()
        gene_set_names <- getGenesetNames()
        
        if (gene_set_types %in% names(ese@gene_set_analyses[[assay]]) && gene_set_names %in% rownames(ese@gene_set_analyses[[assay]][[gene_set_types]][[getSelectedContrasts()]])) {
            fdr <- paste(signif(ese@gene_set_analyses[[assay]][[gene_set_types]][[getSelectedContrasts()]][gene_set_names, 
                "FDR"], 3), collapse = ",")
            direction <- paste(ese@gene_set_analyses[[assay]][[gene_set_types]][[getSelectedContrasts()]][gene_set_names, 
                "Direction"], collapse = ",")
            title_components <- c(title_components, paste(paste("Direction:", direction), paste("FDR:", fdr)))
        } else {
            title_components <- c(title_components, "(no association)")
        }
        
        plot_title <- paste(title_components, collapse = "\n")
        
        plot_title
    })
    
    # Get the list of fold changes by which to rank genes
    
    getFoldChanges <- reactive({
        ct <- filteredContrastsTables()[[1]]
        ct$`Fold change`
    })
    
    # Get gene IDs of the same type as used for gene sets
    
    getGeneIDs <- reactive({
        convertIds(rownames(filteredContrastsTables()[[1]]), getExperiment(), getExperiment()@entrezgenefield)
    })
    
    # Make the barcode plot using limma for download
    
    plotGenesetBarcodeplot <- reactive({
        barcode_plot(getFoldChanges(), getGeneIDs(), names(getPathwayGenes()), barcodeplotTitle())
    })
    
    # Render the barcode plot
    
    output$genesetbarcodeplot <- renderPlot({
        barcode_plot(getFoldChanges(), getGeneIDs(), names(getPathwayGenes()), barcodeplotTitle())
    })
    
    # Make a table of contrast data for the gene set Subset the linked contrasts table for the gene set genes
    
    gsbpContrastsTable <- reactive({
        lct <- labelledContrastsTable()
        ese <- getExperiment()
        set_genes <- getPathwayGenes()
        
        lct[which(lct[[prettifyVariablename(ese@labelfield)]] %in% set_genes), ]
    })
    
    # Add links for display
    
    gsbpLinkedContrastsTable <- reactive({
        linkMatrix(gsbpContrastsTable(), eselist@url_roots)
    })
    
    # Provide the gene set genes in a table of contrst data
    
    callModule(simpletable, "genesetbarcodeplot", downloadMatrix = gsbpContrastsTable, displayMatrix = gsbpLinkedContrastsTable, 
        filename = "gene_set_contrast", rownames = FALSE, pageLength = 10)
    
    # Catch the gene set from the URL
    
    observe({
        query <- parseQueryString(session$clientData$url_search)
        
        if (length(intersect(c("geneset"), names(query))) == 0) {
            return()
        }
        
        url_observe <- observe({
            if ("geneset" %in% names(query)) {
                updateGeneset()
            }
            url_observe$suspend()
        })
    })
    
    updateGeneset
}

#' Make a gene set barcode plot using Limma
#'
#' @param fold_changes A list of fold changes
#' @param gene_ids Gene set IDs for the values in \code{fold_changes}
#' @param set_gene_ids Gene IDs for the gene set
#' @param plot_title A title for the plot
#'
#' @export

barcode_plot <- function(fold_changes, gene_ids, set_gene_ids, plot_title) {
    par(cex = 1.5, cex.main = 0.8, mar = c(4, 0, 4, 0))
    
    limma::barcodeplot(fold_changes, index = gene_ids %in% set_gene_ids, main = plot_title)
} 
