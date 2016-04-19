#' The UI input function of the geneselect module
#' 
#' This module provides controls for selecting genes (matrix rows) by various
#' criteria such as variance and gene set. 
#' 
#' This will generally not be called directly, but by other modules such as the
#' heatmap module.
#'
#' @param id Submodule namespace
#' @param select_genes Disable gene (row) - wise selection if set to FALSE
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' geneselectInput(ns('heatmap'))

geneselectInput <- function(id, select_genes = TRUE) {
    ns <- NS(id)
    
    if (select_genes) {
        uiOutput(ns("geneSelect"))
    } else {
        hiddenInput(ns("geneSelect"), "all")
    }
}

#' The server function of the geneselect module
#' 
#' This module provides controls for selecting genes (matrix rows) by various 
#' criteria such as variance and gene set.
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param getExperiment Reactive expression which returns a 
#'   ExploratorySummarizedExperiment object with assay and experimental data
#' @param var_n The number of rows to select when doing so by variance. Default
#'   = 50
#' @param var_max The maximum umber of rows to select when doing so by variance.
#'   Default = 500
#' @param selectSamples A reactive expression that provides a vector of samples 
#'   to use, e.g. in row-wise variance calculation
#' @param provide_all Allow the 'all rows' selection in the UI? Means we don't
#'   have to calculate variance so the display is quicker, but it's a bad idea
#'   for e.g. heatmaps where the visual scales by the numbre of rows.
#'   
#' @return output A list of reactive functions for interrogating the selected 
#'   rows.
#'   
#' @keywords shiny
#'   
#' @examples
#' geneselect_functions <- callModule(geneselect, 'heatmap', getExperiment, getMatrix=selectColumns)

geneselect <- function(input, output, session, eselist, getExperiment, var_n = 50, var_max = 500, selectSamples = reactive({
    colnames(getExperiment())
}), assay, provide_all = TRUE, provide_none = FALSE, default = NULL) {
    
    # Check if we have the nessary component for gene sets
    
    useGenesets <- reactive({
        ese <- getExperiment()
        
        length(eselist@gene_sets) > 0 & all(unlist(lapply(c("entrezgenefield", "labelfield"), function(x) length(slot(ese, x)) > 0)))
    })
    
    # Grab the gene set functionality from it's module if we need it. We must also have gene sets and a way of mapping them to our results
    
    unpack.list(callModule(genesetselect, "geneset", eselist = eselist, getExperiment = getExperiment))
    
    # Add the gene sets to the drop-down if required
    
    observeEvent(input$geneSelect, {
        if (input$geneSelect == "gene set") {
            updateGeneSetsList()
        }
    })
    
    # Render the geneSelect UI element
    
    output$geneSelect <- renderUI({
        withProgress(message = "Rendering row selection", value = 0, {
            
            ns <- session$ns
            
            gene_select_methods <- c()
            if (provide_none) {
                gene_select_methods <- c("none")
            }
            if (provide_all) {
                gene_select_methods <- c(gene_select_methods, "all")
            }
            
            gene_select_methods <- c(gene_select_methods, c("variance", "list"))
            
            
            if (useGenesets()) {
                gene_select_methods <- c(gene_select_methods, "gene set")
            }
            
            if (is.null(default)) {
                selected = gene_select_methods[1]
            } else {
                selected = default
            }
            
            gene_select <- list(h5("Select genes/ rows"), selectInput(ns("geneSelect"), "Select genes by", gene_select_methods, selected = selected), conditionalPanel(condition = paste0("input['", 
                ns("geneSelect"), "'] == 'variance' "), sliderInput(ns("obs"), "Show top N most variant rows:", min = 10, max = var_max, value = var_n)), conditionalPanel(condition = paste0("input['", 
                ns("geneSelect"), "'] == 'list' "), tags$textarea(id = ns("geneList"), rows = 3, cols = 20, "Paste gene list here, one per line")))
            
            # If gene sets have been provided, then make a gene sets filter
            
            if (useGenesets()) {
                gene_select[[length(gene_select) + 1]] <- conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'gene set' "), genesetselectInput(ns("geneset")))
            }
            
        })
        
        gene_select
    })
    
    # Reactive function to calculate variances only when required
    
    rowVariances <- reactive({
        withProgress(message = "Calculating row variances", value = 0, {
            apply(SummarizedExperiment::assays(getExperiment())[[assay()]][, selectSamples()], 1, var)
        })
    })
    
    # Make all the reactive expressions that will be needed by calling modules.
    
    geneselect_functions <- list()
    
    # Main output. Derive the expression matrix according to row-based criteria
    
    geneselect_functions$selectRows <- reactive({
        
        ese <- getExperiment()
        
        withProgress(message = "Selecting rows", value = 0, {
            validate(need(!is.null(input$geneSelect), "Waiting for geneSelect"))
            
            if (input$geneSelect == "none") {
                return(c())
            } else if (input$geneSelect == "all") {
                return(rownames(ese))
            } else if (input$geneSelect == "variance") {
                return(rownames(ese)[order(rowVariances(), decreasing = TRUE)[1:input$obs]])
            } else {
                if (input$geneSelect == "gene set") {
                  selected_genes <- getPathwayGenes()
                } else {
                  selected_genes <- unlist(strsplit(input$geneList, "\\n"))
                }
                
                # Use annotation for gene names if specified, otherwise use matrix rows
                
                if (length(ese@labelfield) > 0) {
                  annotation <- data.frame(mcols(ese))
                  selected_rows <- as.character(annotation[which(tolower(annotation[[ese@labelfield]]) %in% tolower(selected_genes)), ese@idfield])
                } else {
                  selected_rows <- rownames(ese)[which(tolower(rownames(ese))) %in% tolower(selected_genes)]
                }
                
                return(rownames(ese)[rownames(ese) %in% selected_rows])
            }
        })
    })
    
    # Make a title
    
    geneselect_functions$title <- reactive({
        
        validate(need(!is.null(input$geneSelect), "Waiting for form to provide geneSelect"))
        
        title <- ""
        if (input$geneSelect == "all") {
            title <- "All rows"
        } else if (input$geneSelect == "variance") {
            title <- paste(paste("Top", input$obs, "rows"), "by variance")
        } else if (input$geneSelect == "gene set") {
            title <- paste0("Genes in sets:\n", paste(prettifyGeneSetName(getPathwayNames()), collapse = "\n"))
        } else if (input$geneSelect == "list") {
            title <- "Rows for specifified gene list"
        }
        title
    })
    
    geneselect_functions
} 
