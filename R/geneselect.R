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
#' StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#' @param var_n The number of rows to select when doing so by variance. Default = 50
#' @param var_max The maximum umber of rows to select when doing so by variance. 
#' Default = 500
#' @param selectSamples A reactive expression that provides a vector of samples
#' to use, e.g. in row-wise variance calculation
#'
#' @return output A list of reactive functions for interrogating the selected
#' rows.
#'
#' @keywords shiny
#' 
#' @examples
#' geneselect_functions <- callModule(geneselect, 'heatmap', getExperiment, getMatrix=selectColumns)

geneselect <- function(input, output, session, getExperiment, var_n = 50, var_max = 500, selectSamples, assay) {
    
    observe({
        se <- getExperiment()
    })
    
    # Check if we're using annotation
    
    use_annotation <- all(c(nrow(mcols(se)) > 0, c("transcriptfield", "genefield") %in% names(metadata(se))))
    
    # Check if we have the nessary component for gene sets
    
    use_genesets <- all(c("geneset_files", "entrezgenefield", "genefield") %in% names(metadata(se)))
    
    # Render the geneSelect UI element
    
    output$geneSelect <- renderUI({
        
        ns <- session$ns
        
        gene_select_methods <- c("variance", "list")
        if (use_genesets) {
            gene_select_methods <- c(gene_select_methods, "gene set")
        }
        
        gene_select <- list(h5("Select genes/ rows"), selectInput(ns("geneSelect"), "Select genes by", gene_select_methods, selected = "variance"), conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'variance' "), 
            sliderInput(ns("obs"), "Show top N most variant rows:", min = 10, max = var_max, value = var_n)), conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'list' "), tags$textarea(id = ns("geneList"), rows = 3, 
            cols = 30, "Paste gene list here, one per line")))
        
        # If gene sets have been provided, then make a gene sets filter
        
        if (use_genesets) {
            gene_select[[length(gene_select) + 1]] <- conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'gene set' "), genesetInput(ns("heatmap")))
        }
        
        gene_select
    })
    
    # Grab the gene set functionality from it's module if we need it. We must also have gene sets and a way of mapping them to our results
    
    if (use_genesets) {
        geneset_functions <- callModule(geneset, "heatmap", data.frame(mcols(se)), se$entrezgenefield, se$genefield, se$geneset_files)
    }
    
    # Reactive function to calculate variances only when required
    
    rowVariances <- reactive({
        withProgress(message = "Calculating row variances", value = 0, {
            apply(GenomicRanges::assays(se)[[assay()]][, selectSamples()], 1, var)
        })
    })
    
    # Make all the reactive expressions that will be needed by calling modules.
    
    geneselect_functions <- list()
    
    # Main output. Derive the expression matrix according to row-based criteria
    
    geneselect_functions$selectRows <- reactive({
        
        validate(need(!is.null(input$geneSelect), "Waiting for form to provide geneSelect"))
        
        if (input$geneSelect == "all") {
            return(rownames(se))
        } else if (input$geneSelect == "variance") {
            return(rownames(se)[order(rowVariances(), decreasing = TRUE)[1:input$obs]])
        } else {
            if (input$geneSelect == "gene set") {
                selected_genes <- geneset_functions$getPathwayGenes()
            } else {
                selected_genes <- unlist(strsplit(input$geneList, "\\n"))
            }
            
            # Use annotation for gene names if specified, otherwise use matrix rows
            
            if ("genefield" %in% names(metadata(se))) {
                annotation <- data.frame(mcols(se))
                selected_rows <- as.character(annotation[which(tolower(annotation[[metadata(se)$genefield]]) %in% tolower(selected_genes)), metadata(se)$transcriptfield])
            } else {
                selected_rows <- rownames(se)[which(tolower(rownames(se))) %in% tolower(selected_genes)]
            }
            
            return(rownames(se)[rownames(se) %in% selected_rows])
        }
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
            title <- paste0("Genes in sets:\n", paste(geneset_functions$getPathwayNames(), collapse = "\n"))
        } else if (input$geneSelect == "list") {
            title <- "Rows for specifified gene list"
        }
        title
    })
    
    geneselect_functions
} 
