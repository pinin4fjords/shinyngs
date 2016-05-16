#' The input function of the gene module
#' 
#' The gene module picks specified rows out the assay data, either simply by id
#' or label. This is used to create a gene-centric info page.
#' 
#' Inputs are a gene label and a variable to color by (where available)
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
#' geneInput(ns('gene'), eselist)

geneInput <- function(id, eselist) {
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("gene"), eselist)
    gene_filters <- list(
      selectizeInput(ns("gene_label"), "Gene label", choices = NULL, options = list(placeholder = "Type a gene label", maxItems = 5)), 
        groupbyInput(ns("gene")))
    
    field_sets = list(gene = gene_filters)
    naked_fields = list()  # Things we don't want to wrap in a field set - probably hidden stuff
    
    # Don't create an empty field set if we're not grouping
    
    if (singleValidMatrix(eselist)) {
        naked_fields[[1]] <- expression_filters
    } else {
        field_sets$expression <- expression_filters
    }
    
    field_sets <- c(field_sets, list(table_options = contrastsInput(ns("gene"), allow_filtering = FALSE)))
    
    list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The input function of the gene module
#' 
#' The gene module picks specified rows out the assay data, either simply by id
#' or label. This is used to create a gene-centric info page.
#' 
#' Outputs are a bar plot and a table contrast data for this gene
#' 
#' @param id Submodule namespace
#' @param dses List of structuredExperiment objects with assay and experimental 
#'   data
#'   
#' @return output An HTML tag object that can be rendered as HTML using 
#'   as.character()
#'   
#' @keywords shiny
#'   
#' @examples
#' geneOutput(ns('gene'), eselist)

geneOutput <- function(id, eselist) {
    ns <- NS(id)
    
    list(
      uiOutput(ns('info')),
      
      uiOutput(ns('title')), plotlyOutput(ns("barPlot"), height = 500), h4("Contrasts table"), DT::dataTableOutput(ns("contrastTable"))
    )
}

#' The server function of the gene module
#' 
#' The gene module picks specified rows out the assay data, either simply by id
#' or label. This is used to create a gene-centric info page.
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
#' callModule(gene, 'gene', eselist)

gene <- function(input, output, session, eselist) {
    
    # Call all the required modules and unpack their reactives
    
    unpack.list(callModule(selectmatrix, "gene", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = FALSE))
    unpack.list(callModule(contrasts, "gene", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, 
        multiple = TRUE, show_controls = FALSE))
    colorBy <- callModule(groupby, "gene", eselist = eselist, group_label = "Color by")
    
    # The title and info link are reactive to the currently active experiment
    
    output$title <- renderUI({
      en <- getExperimentName()
      h3(paste(en, "expression bar plot"))
    })
    
    output$info <- renderUI({
      ns <- session$ns
      en <- getExperimentName()
      
      list(
        modalInput(ns("geneInfo"), paste(en, " info"), "help"),
        modalOutput(ns("geneInfo"), paste(en, "information"), DT::dataTableOutput(ns('geneInfoTable')))
      )
    })
    
    # Get the list of valid IDs / labels. This will be used to populate the autocomplete field
    
    getGeneNames <- reactive({
        ese <- getExperiment()
        
        gene_names <- rownames(ese)
        if (length(ese@labelfield) > 0) {
            gene_names <- sort(mcols(ese)[[ese@labelfield]])
        }
    })
    
    # A static version to stop the gene box getting reset when changing between transcript/gene.  But dynamic might be desirable where genes are
    # different between assays.....
    
    getGeneNamesStatic <- function() {
        gene_name_lists <- lapply(eselist, function(ese) {
            mcols(ese)[[ese@labelfield]]
        })
        
        sort(Reduce(union, gene_name_lists))
    }
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list
    
    observe({
        updateSelectizeInput(session, "gene_label", choices = getGeneNamesStatic(), server = TRUE)
    })
    
    # get the gene label
    
    getGeneLabels <- reactive({
        validate(need(!is.null(input$gene_label), "Waiting for a gene input"))
        input$gene_label
    })
    
    # Get the row or rows of the data that correspond to this symbol
    
    getRows <- reactive({
        rowids <- getGeneLabels()
        ese <- getExperiment()

        if (length(ese@labelfield) > 0) {
            
            gene_labels <- getGeneLabels()
            
            # The rows of the entire object
            
            rowids <- rownames(ese)[which(mcols(ese)[[ese@labelfield]] %in% gene_labels)]
        }
 
        rowids
    })
    
    # Get the rows with valid data. Some rows of some assays may be blank
    
    getRowsWithData <- reactive({
        rowids <- getRows()
        
        sm <- selectMatrix()
        rowids <- rowids[rowids %in% rownames(sm)]
        gene_labels <- getGeneLabels()
        
        validate(need(length(rowids) > 0, paste0("No values for gene labels '", paste(gene_labels, collapse = "', '"), "' in assay '", getAssay(), "'")))
        
        rowids
    })
    
    # Render the bar plot with plotly
    
    output$barPlot <- renderPlotly({
        
        withProgress(message = "Making bar plot", value = 0, {
            ese <- getExperiment()
            rows <- getRowsWithData()
            
            barplot_expression <- selectMatrix()[rows, , drop = FALSE]
            
            if (length(ese@labelfield) > 0) {
                rownames(barplot_expression) <- idToLabel(rows, ese, sep = "<br />")
            }
            
            p <- geneBarplot(barplot_expression, selectColData(), colorBy(), getAssayMeasure())
            
        })
    })

    # Make a table of the annotation data
    
    output$geneInfoTable <- DT::renderDataTable({
      rows <- getRows()
      ese <- getExperiment()
      
      gene_info <- data.frame(mcols(ese[rows, , drop = FALSE]), check.names = FALSE, row.names = idToLabel(rows, ese, sep = " /<br/ >"))
      gene_info <- t(linkMatrix(gene_info, eselist@url_roots))
      rownames(gene_info) <- prettifyVariablename(rownames(gene_info))
      gene_info
      
    }, options = list(rownames = TRUE, pageLength = 20, dom = 't'), escape = FALSE)
    
    # Retrieve the contrasts table
    
    getGeneContrastsTable <- reactive({
        rows <- getRowsWithData()
        contrasts_table <- labelledContrastsTable()

        linkMatrix(contrasts_table[contrasts_table[[prettifyVariablename(getIdField())]] %in% rows, , drop = FALSE], url_roots = eselist@url_roots)
    })
    
    # Render the contrasts table- when a valid label is supplied
    
    if (length(eselist@contrasts) > 0) {
        output$contrastTable <- DT::renderDataTable({
            getGeneContrastsTable()
        }, rownames = FALSE, escape = FALSE)
    }
    
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
    
    if (!is.null(colorby)) {
        groups <- as.character(experiment[colnames(expression), colorby])
        groups[is.na(groups)] <- "N/A"
    }
    
    # A hidden axis
    
    ax <- list(title = "", showline = FALSE, showticklabels = FALSE, range = list(0, max(expression) * 1.05))
    
    plots <- lapply(1:nrow(expression), function(rowno) {
        row <- expression[rowno, ]
        
        if (rowno == 1) {
            yaxis = list(title = expressionmeasure, range = list(0, max(expression) * 1.05))
        } else {
            yaxis = ax
        }
        
        plotargs <- list(x = paste0(names(row), "&nbsp;"), y = as.numeric(row), type = "bar", showlegend = (rowno == 1), evaluate = TRUE)
        
        if (!is.null(colorby)) {
            plotargs$color <- groups
        }
        
        do.call(plot_ly, plotargs) %>% layout(xaxis = list(title = rownames(expression)[rowno], titlefont = list(size = 10)), yaxis = yaxis, margin = list(b = 150), 
            evaluate = TRUE)
    })
    
    if (length(plots) > 1) {
        p <- do.call(function(...) subplot(...), plots)
    } else {
        p <- plots[[1]]
    }
    p %>% config(showLink = TRUE)
} 
