#' The input function of the gene module
#' 
#' The gene module picks specified rows out the assay data, either simply by 
#' id or label. This is used to create a gene-centric info page.
#' 
#' Inputs are a gene label and a variable to color by (where available)
#'
#' @param id Submodule namespace
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' geneInput(ns('gene'), ses)

geneInput <- function(id, ses) {
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("gene"), ses)
    gene_filters <- list(selectizeInput(ns("gene_label"), "Gene label", choices = NULL, options = list(placeholder = "Type a gene label", maxItems = 5)), groupbyInput(ns("gene")))
    
    list(expression_filters, fieldSets(ns("fieldset"), list(gene = gene_filters, table_options = contrastsInput(ns("gene"), allow_filtering = FALSE))))
    
}

#' The input function of the gene module
#' 
#' The gene module picks specified rows out the assay data, either simply by 
#' id or label. This is used to create a gene-centric info page.
#' 
#' Outputs are a bar plot and a table contrast data for this gene
#'
#' @param id Submodule namespace
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' geneOutput(ns('gene'), ses)

geneOutput <- function(id, ses) {
    ns <- NS(id)
    
    list(h4("Barplot"), plotlyOutput(ns("barPlot"), height = 500), h4("Contrasts table"), DT::dataTableOutput(ns("contrastTable")))
}

#' The server function of the gene module
#' 
#' The gene module picks specified rows out the assay data, either simply by 
#' id or label. This is used to create a gene-centric info page.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(gene, 'gene', ses)

gene <- function(input, output, session, ses) {
    
    # Call all the required modules and unpack their reactives
    
    unpack.list(callModule(selectmatrix, "gene", ses, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = FALSE))
    unpack.list(callModule(contrasts, "gene", getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE, show_controls = FALSE))
    colorBy <- callModule(groupby, "gene", getExperiment = getExperiment, group_label = "Color by")
    
    # Get the list of valid IDs / labels. This will be used to populate the autocomplete field
    
    getGeneNames <- reactive({
        se <- getExperiment()
        
        gene_names <- rownames(se)
        if ("labelfield" %in% names(metadata(se))) {
            gene_names <- sort(mcols(se)[[metadata(se)$labelfield]])
        }
    })
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list
    
    observe({
        updateSelectizeInput(session, "gene_label", choices = getGeneNames(), server = TRUE)
    })
    
    # Get the row or rows of the data that correspond to this symbol
    
    getRows <- reactive({
        rowids <- input$gene_label
        if ("labelfield" %in% names(metadata(se))) {
            rowids <- rownames(se)[mcols(se)[[metadata(se)$labelfield]] == input$gene_label]
        }
    })
    
    # Render the bar plot with plotly
    
    output$barPlot <- renderPlotly({
        validate(need(!is.null(input$gene_label), "Waiting for a gene input"))
        
        withProgress(message = "Making bar plot", value = 0, {
            se <- getExperiment()
            barplot_expression <- selectMatrix()[getRows(), , drop = FALSE]
            
            if ("labelfield" %in% names(metadata(se))) {
                rownames(barplot_expression) <- paste(getRows(), input$gene_label, sep = " / ")
            }
            
            p <- geneBarplot(barplot_expression, selectColData(), colorBy(), "expression")
            
        })
    })
    
    # Convenience function for deciding whether to use ID or symbol
    
    getLabelField <- reactive({
        se <- getExperiment()
        if ("labelfield" %in% names(metadata(se))) {
            metadata(se)$labelfield
        } else {
            metadata(se)$idfield
        }
    })
    
    # Retrieve the contrasts table
    
    getGeneContrastsTable <- reactive({
        contrasts_table <- labelledContrastsTable()
        saveRDS(contrasts_table, file='~/shinytests/contrasts_table.rds')
        linkMatrix(contrasts_table[contrasts_table[[prettifyVariablename(getLabelField())]] == input$gene_label, , drop = FALSE], getExperiment())
    })
    
    # Render the contrasts table- when a valid label is supplied
    
    output$contrastTable <- DT::renderDataTable({
        validate(need(!is.null(input$gene_label), "Waiting for a gene input"))
        
        getGeneContrastsTable()
    }, rownames = FALSE, escape = FALSE)
    
    # Supply a reactive for updating the gene input field
    
    reactive({
        query <- parseQueryString(session$clientData$url_search)
        updateSelectizeInput(session, "gene_label", selected = query$gene, choices = getGeneNames(), server = TRUE)
    })
    
}

#' Main function for drawing the bar plot with plotly
#' 
#' The gene module picks specified rows out the assay data, either simply by 
#' id or label. This is used to create a gene-centric info page.
#' 
#' This function does the actual plotting with plotly. It will produce a plot 
#' for every row of the input matrix.
#' 
#' @param expression Matrix of values
#' @param experiment Data frame containing metadata to use for coloring etc
#' @param colorby Column name in \code{experiment} specifying how points should be colored
#' @param expressionmeasure String to use for labelling y axis in plots 
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(gene, 'gene', ses)

geneBarplot <- function(expression, experiment, colorby, expressionmeasure = "Expression") {
    
    groups <- as.character(experiment[colnames(expression), colorby])
    groups[is.na(groups)] <- "N/A"
    
    # A hidden axis
    
    ax <- list(title = "", showline = FALSE, showticklabels = FALSE, range = list(0, max(expression) * 1.05))
    
    plots <- lapply(1:nrow(expression), function(rowno) {
        row <- expression[rowno, ]
        
        if (rowno == 1) {
            yaxis = list(title = expressionmeasure, range = list(0, max(expression) * 1.05))
        } else {
            yaxis = ax
        }
        
        plot_ly(x = names(row), y = as.numeric(row), type = "bar", color = groups, showlegend = (rowno == 1)) %>% layout(xaxis = list(title = rownames(expression)[rowno]), yaxis = yaxis, 
            margin = list(b = 100))
    })
    
    do.call(function(...) subplot(...), plots)
    
} 
