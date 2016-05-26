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
      labelselectfieldInput(ns("gene_label")),
      #selectizeInput(ns("gene_label"), "Gene label", choices = NULL, options = list(placeholder = "Type a gene label", maxItems = 5)), 
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
    
    out <- list()
    
    # if (length(eselist@ensembl_species) > 0){
    #   out <- list(
    #     modalInput(ns("geneModel"), 'Gene model', "help"),
    #     modalOutput(ns("geneModel"), 'Gene model', plotOutput(ns('geneModel')))
    #   )
    # }
    
    out <- c(out, list(
      uiOutput(ns('model')),
      uiOutput(ns('info')),
      uiOutput(ns('title')), plotlyOutput(ns("barPlot"), height = 500), h4("Contrasts table"), DT::dataTableOutput(ns("contrastTable"))
    ))
    
    out
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
    unpack.list(callModule(labelselectfield, 'gene_label', eselist = eselist, getExperiment = getExperiment, labels_from_all_experiments = TRUE, url_field = 'gene'))
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
      gene_labels <- getSelectedLabels()
      
      list(
        modalInput(ns("geneInfo"), paste(en, " info"), "help"),
        modalOutput(ns("geneInfo"), paste(en, "information for", paste(gene_labels, sep = ', ')), DT::dataTableOutput(ns('geneInfoTable')))
      )
    })
    
    # Render the gene model plot 
    
    output$model <- renderUI({
      ns <- session$ns
      gene_labels <- getSelectedLabels()
      
      if (length(eselist@ensembl_species) > 0){
        out <- list(
          modalInput(ns("geneModel"), 'Gene model', "help"),
          modalOutput(ns("geneModel"), paste(gene_labels[1], 'gene model'), plotOutput(ns('geneModel'), height = "600px"))
        )
      }
    })
    
    # Get the rows with valid data. Some rows of some assays may be blank

    getSelectedIdsWithData <- reactive({
        rowids <- getSelectedIds()

        sm <- selectMatrix()
        rowids <- rowids[rowids %in% rownames(sm)]
        gene_labels <- getSelectedLabels()

        validate(need(length(rowids) > 0, paste0("No values for gene labels '", paste(gene_labels, collapse = "', '"), "' in assay '", getAssay(), "'")))

        rowids
    })
    
    # Render the bar plot with plotly
    
    output$barPlot <- renderPlotly({
        
        withProgress(message = "Making bar plot", value = 0, {
            ese <- getExperiment()
            rows <- getSelectedIdsWithData()
            
            barplot_expression <- selectMatrix()[rows, , drop = FALSE]
            
            if (length(ese@labelfield) > 0) {
                rownames(barplot_expression) <- idToLabel(rows, ese, sep = "<br />")
            }
            
            p <- geneBarplot(barplot_expression, selectColData(), colorBy(), getAssayMeasure())
            
        })
    })

    # Make a gene region plot
    
    output$geneModel <- renderPlot({ 

      gene_labels <- getSelectedLabels()
      
      withProgress(message = paste("Fetching gene models from Ensembl for gene", gene_labels[1]), value = 0, {
        annotation <- data.frame(SummarizedExperiment::mcols(eselist$gene), stringsAsFactors = FALSE)
        annotation <- annotation[which(annotation[[eselist$gene@labelfield]] == gene_labels[1]),]
        
        geneModelPlot(ensembl_species = eselist@ensembl_species, chromosome = annotation$chromosome_name, start = annotation$start_position, end = annotation$end_position)
      })
    })
    
    # Make a table of the annotation data
    
    output$geneInfoTable <- DT::renderDataTable({
      rows <- getSelectedIds()
      ese <- getExperiment()
      
      gene_info <- data.frame(mcols(ese[rows, , drop = FALSE]), check.names = FALSE, row.names = idToLabel(rows, ese, sep = " /<br/ >"))
      gene_info <- t(linkMatrix(gene_info, eselist@url_roots))
      rownames(gene_info) <- prettifyVariablename(rownames(gene_info))
      gene_info
      
    }, options = list(rownames = TRUE, pageLength = 20, dom = 't'), escape = FALSE)
    
    # Make the gene info table update (probably invisibly) even when hidden, so
    # there's not a delay in rendering when the link to the modal is clicked.
    
    outputOptions(output, 'geneInfoTable', suspendWhenHidden = FALSE)
    
    # Retrieve the contrasts table
    
    getGeneContrastsTable <- reactive({
        rows <- getSelectedIdsWithData()
        contrasts_table <- labelledContrastsTable()

        linkMatrix(contrasts_table[contrasts_table[[prettifyVariablename(getIdField())]] %in% rows, , drop = FALSE], url_roots = eselist@url_roots)
    })
    
    # Render the contrasts table- when a valid label is supplied
    
    if (length(eselist@contrasts) > 0) {
        output$contrastTable <- DT::renderDataTable({
            getGeneContrastsTable()
        }, rownames = FALSE, escape = FALSE)
    }
    
    # Return the reactive for updating the gene input field. Will be used for
    # updating the field when linking to this panel
    
    updateLabelField
    
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


#' Make a gene model plot for a chromosomal location
#'
#' @param ensembl_species 
#' @param chromosome 
#' @param start 
#' @param end 
#'
#' @import Gviz

geneModelPlot <- function(ensembl_species, chromosome, start, end){
  
  # Initialise a connection to Ensembl
  
  ensembl <-
    biomaRt::useMart(
      biomart = "ENSEMBL_MART_ENSEMBL",
      dataset = paste0(ensembl_species, '_gene_ensembl'),
      host = 'www.ensembl.org'
    )
  
  options(ucscChromosomeNames=FALSE)
  
  # Create a basic axis
  
  gtrack <- GenomeAxisTrack()
  
  # We should know the start_position and end_position. Fetch a track showing genes in the region
  
  geneTrack <- BiomartGeneRegionTrack(
    #genome = genome,
    chromosome = chromosome,
    start = start,
    end = end,
    name = "Gene",
    biomart = ensembl,
    transcriptAnnotation = "symbol",
    collapseTranscripts = TRUE,
    shape = "arrow"
  )
  
  # Move gene labels to above
  
  displayPars(geneTrack) = list(showId = TRUE,
                                fontcolor.title = "black",
                                just.group = "above")
  
  transcriptTrack <- BiomartGeneRegionTrack(
    #genome = genome,
    chromosome = chromosome,
    start = start,
    end = end,
    name = "Transcripts",
    biomart = ensembl,
    transcriptAnnotation = "transcript"
  )
  
  # Move transcript labels to above
  
  displayPars(transcriptTrack) = list(showId = TRUE,
                                      fontcolor.title = "black",
                                      just.group = "above")
  
  plotTracks(
    list(gtrack, geneTrack, transcriptTrack),
    from = start,
    to = end,
    extend.left = 1000,
    extend.right = 1000,
    cex = 1,
    cex.legend = 0.8,
    cex.group = 0.8,
    cex.title = 0.8
  )
}


