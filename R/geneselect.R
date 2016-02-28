#' The UI input function of the geneselect module
#' 
#' This module provides controls for selecting genes (matrix rows) by various
#' criteria such as variance and gene set. 
#' 
#' This will generally not be called directly, but by other modules such as the
#' heatmap module.
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' geneselectInput(ns('heatmap'))

geneselectInput <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns("geneSelect"))
}

#' The server function of the geneselect module
#' 
#' This module provides controls for selecting genes (matrix rows) by various
#' criteria such as variance and gene set. 
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param transcriptfield The main identifier for the rows in the assay data.
#' This could be transcript ID, but also probe etc
#' @param entrezgenefield The column of annotation containing Entrez gene IDs
#' @param genefield The gene ID type in annotation by which results are keyed
#' @param geneset_files (optional) A named list of .gmt gene set files as might be 
#' derived from MSigDB
#' @param var_n The number of rows to select when doing so by variance. Default = 50
#' @param selectSamples A reactive expression that provides a vector of samples
#' to use, e.g. in row-wise variance calculation
#'
#' @return output A list of reactive functions for interrogating the selected
#' rows.
#'
#' @keywords shiny
#' 
#' @examples
#' geneselect_functions <- callModule(geneselect, 'heatmap', se, transcriptfield, entrezgenefield, genefield, geneset_files, getMatrix=selectColumns)

geneselect <- function(input, output, session, se, transcriptfield, entrezgenefield, genefield, geneset_files = NULL, var_n = 50, selectSamples, 
    assay) {
    
    # Render the geneSelect UI element
    
    output$geneSelect <- renderUI({
        
        ns <- session$ns
        
        gene_select_methods <- c("variance", "list")
        if (!is.null(geneset_files)) {
            gene_select_methods <- c(gene_select_methods, "gene set")
        }
        
        gene_select <- list(h4("Select genes/ rows"), selectInput(ns("geneSelect"), "Select genes by", gene_select_methods, selected = "variance"), 
            conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'variance' "), sliderInput(ns("obs"), "Show top N most variant rows:", 
                min = 10, max = nrow(se), value = var_n)), conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'list' "), tags$textarea(id = ns("geneList"), 
                rows = 3, cols = 30, "Paste gene list here, one per line")))
        
        # If gene sets have been provided, then make a gene sets filter
        
        if (!is.null(geneset_files)) {
            gene_select[[length(gene_select) + 1]] <- conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'gene set' "), genesetInput(ns("heatmap")))
        }
        
        gene_select
    })
    
    # Grab the gene set functionality from it's module if we need it
    
    if (!is.null(geneset_files)) {
        geneset_functions <- callModule(geneset, "heatmap", data.frame(mcols(se)), entrezgenefield, genefield, geneset_files)
    }
    
    # Reactive function to calculate variances only when required
    
    rowVariances <- reactive({
        withProgress(message = "Calculating row variances", value = 0, {
            apply(assays(se)[[assay()]][, selectSamples()], 1, var)
        })
    })
    
    # Make all the reactive expressions that will be needed by calling modules.
    
    geneselect_functions <- list()
    
    # Main output. Derive the expression matrix according to row-based criteria
    
    geneselect_functions$selectRows <- reactive({
        
        validate(need(!is.null(input$geneSelect), "Waiting for form to provide geneSelect"))
        
        if (input$geneSelect == "variance") {
            return(rownames(se)[order(rowVariances(), decreasing = TRUE)[1:input$obs]])
        } else {
            if (input$geneSelect == "gene set") {
                selected_genes <- geneset_functions$getPathwayGenes()
            } else {
                selected_genes <- unlist(strsplit(input$geneList, "\\n"))
            }
            
            selected_rows <- as.character(annotation[which(tolower(annotation[[genefield]]) %in% tolower(selected_genes)), transcriptfield])
            return(rownames(se)[rownames(se) %in% selected_rows])
        }
    })
    
    # Make a title
    
    geneselect_functions$title <- reactive({
        
        validate(need(!is.null(input$geneSelect), "Waiting for form to provide geneSelect"))
        
        title <- ""
        if (input$geneSelect == "variance") {
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
