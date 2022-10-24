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
#' geneInput(ns("gene"), eselist)
#'
geneInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("gene"), eselist)
  gene_filters <- list(labelselectfieldInput(ns("gene_label"), id_selection = TRUE), groupbyInput(ns("gene")))

  field_sets <- list(gene = gene_filters)
  naked_fields <- list() # Things we don't want to wrap in a field set - probably hidden stuff

  # Don't create an empty field set if we're not grouping

  if (singleValidMatrix(eselist)) {
    naked_fields[[1]] <- expression_filters
  } else {
    field_sets$expression <- expression_filters
  }

  field_sets <- c(field_sets, list(table_options = contrastsInput(ns("gene"), allow_filtering = FALSE, select_summary_type = TRUE), export = simpletableInput(ns("geneContrastsTable"))))

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
#' @param eselist List of structuredExperiment objects with assay and experimental
#'   data
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' geneOutput("gene", eselist)
#'
geneOutput <- function(id, eselist) {
  ns <- NS(id)

  out <- list()

  out <- c(out, list(
    uiOutput(ns("model")), uiOutput(ns("info")), uiOutput(ns("title")), plotlyOutput(ns("barPlot"), height = 500), h4("Contrasts table"),
    simpletableOutput(ns("geneContrastsTable"))
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
#' callModule(gene, "gene", eselist)
#'
gene <- function(input, output, session, eselist) {
  # Call all the required modules and unpack their reactives

  selectmatrix_reactives <- callModule(selectmatrix, "gene", eselist, var_n = 1000, select_samples = TRUE, select_genes = FALSE, provide_all_genes = FALSE)
  unpack.list(selectmatrix_reactives)
  unpack.list(callModule(labelselectfield, "gene_label",
    eselist = eselist, getExperiment = getExperiment, labels_from_all_experiments = TRUE, url_field = "gene",
    id_selection = TRUE, getNonEmptyRows = getNonEmptyRows
  ))
  unpack.list(callModule(contrasts, "gene", eselist = eselist, multiple = TRUE, show_controls = FALSE, selectmatrix_reactives = selectmatrix_reactives, select_all_contrasts = TRUE))
  unpack.list(callModule(groupby, "gene", eselist = eselist, group_label = "Color by", selectColData = selectColData))

  # The title and info link are reactive to the currently active experiment

  output$title <- renderUI({
    en <- getExperimentName()
    h3(paste(en, "expression bar plot"))
  })

  output$info <- renderUI({
    ns <- session$ns
    en <- getExperimentName()
    gene_labels <- getSelectedLabels()

    list(modalInput(ns("geneInfo"), paste(en, " info"), "help"), modalOutput(
      ns("geneInfo"), paste(en, "information for", paste(gene_labels, sep = ", ")),
      DT::dataTableOutput(ns("geneInfoTable"))
    ))
  })

  # Render the gene model plot

  output$model <- renderUI({
    ns <- session$ns
    gene_labels <- getSelectedLabels()

    if (length(eselist@ensembl_species) > 0) {
      out <- list(modalInput(ns("geneModel"), "Gene model", "help"), modalOutput(ns("geneModel"), paste(gene_labels[1], "gene model"), plotOutput(ns("geneModel"),
        height = "600px"
      )))
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

      p <- geneBarplot(barplot_expression, selectColData(), getGroupby(), getAssayMeasure(), palette = getPalette())
    })
  })

  # Make a gene region plot

  output$geneModel <- renderPlot({
    gene_labels <- getSelectedLabels()
    ese <- getExperiment()

    withProgress(message = paste("Fetching gene models from Ensembl for gene", gene_labels[1]), value = 0, {
      annotation <- data.frame(SummarizedExperiment::mcols(ese), stringsAsFactors = FALSE)
      annotation <- annotation[which(annotation[[ese@labelfield]] == gene_labels[1]), ]

      geneModelPlot(ensembl_species = eselist@ensembl_species, chromosome = annotation$chromosome_name, start = min(annotation$start_position), end = max(annotation$end_position))
    })
  })

  # Make a table of the annotation data

  output$geneInfoTable <- DT::renderDataTable(
    {
      rows <- getSelectedIds()
      ese <- getExperiment()

      validate(need(all(rows %in% rownames(ese)), FALSE))

      gene_info <- data.frame(SummarizedExperiment::mcols(ese[rows, , drop = FALSE]), check.names = FALSE, row.names = idToLabel(rows, ese, sep = " /<br/ >"))
      gene_info <- t(linkMatrix(gene_info, eselist@url_roots))
      rownames(gene_info) <- prettifyVariablename(rownames(gene_info))
      gene_info
    },
    options = list(rownames = TRUE, pageLength = 20, dom = "t"),
    escape = FALSE
  )

  # Make the gene info table update (probably invisibly) even when hidden, so there's not a delay in rendering when the link to the modal is clicked.

  outputOptions(output, "geneInfoTable", suspendWhenHidden = FALSE)

  # Retrieve the contrasts table

  getGeneContrastsTable <- reactive({
    rows <- getSelectedIdsWithData()
    contrasts_table <- labelledContrastsTable()

    contrasts_table[contrasts_table[[prettifyVariablename(getIdField())]] %in% rows, , drop = FALSE]
  })

  # Link the contrasts table for display

  getLinkedGeneContrastsTable <- reactive({
    linkMatrix(getGeneContrastsTable(), url_roots = eselist@url_roots)
  })

  # Render the contrasts table- when a valid label is supplied

  callModule(simpletable, "geneContrastsTable",
    downloadMatrix = getGeneContrastsTable, displayMatrix = getLinkedGeneContrastsTable, filename = "gene_contrasts",
    rownames = FALSE
  )

  # Return the reactive for updating the gene input field. Will be used for updating the field when linking to this panel

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
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressionmeasure String to use for labelling y axis in plots
#'
#' @keywords shiny
#'
#' @examples
#' callModule(gene, "gene", ses)
#'
geneBarplot <- function(expression, experiment, colorby, expressionmeasure = "Expression", palette = NULL) {
  if (!is.null(colorby)) {
    groups <- as.character(experiment[colnames(expression), colorby])
    groups[is.na(groups)] <- "N/A"

    # Convert to factor and maintain order of levels

    groups <- factor(groups, levels = unique(groups))
  }

  # How much space do we need on the x axis?

  xlab_space <- max(unlist(lapply(rownames(experiment), nchar))) * 10

  # A hidden axis

  ax <- list(title = "", showline = FALSE, showticklabels = FALSE, range = list(0, max(expression) * 1.05))

  plots <- lapply(1:nrow(expression), function(rowno) {
    row <- expression[rowno, ]

    if (rowno == 1) {
      yaxis <- list(title = expressionmeasure, range = list(0, max(expression) * 1.05))
    } else {
      yaxis <- ax
    }

    plotargs <- list(x = paste0(names(row), "&nbsp;"), y = as.numeric(row), type = "bar", showlegend = (rowno == 1))

    if (!is.null(colorby)) {
      plotargs$color <- groups
      plotargs$colors <- palette
    }

    do.call(plot_ly, plotargs) %>% layout(xaxis = list(
      categoryarray = rownames(experiment), categoryorder = "array", title = rownames(expression)[rowno],
      titlefont = list(size = 10)
    ), yaxis = yaxis, margin = list(b = xlab_space))
  })

  if (length(plots) > 1) {
    p <- do.call(function(...) subplot(..., titleX = TRUE, titleY = TRUE, shareY = TRUE, shareX = TRUE, nrows = ceiling(length(plots) / 3)), plots)
  } else {
    p <- plots[[1]]
  }
  p %>% config(showLink = TRUE)
}


#' Make a gene model plot for a chromosomal location
#'
#' Uses the Gviz module to show transcripts and exon locations for a given gene
#' id.
#'
#' @param ensembl_species Ensembl species definition like 'mmuscululus'
#' @param chromosome Chromosome number
#' @param start Chromosome start coordinate
#' @param end Chromosome end coordinate

geneModelPlot <- function(ensembl_species, chromosome, start, end) {
  # Initialise a connection to Ensembl

  ensembl <- biomaRt::useMart(biomart = "ENSEMBL_MART_ENSEMBL", dataset = paste0(ensembl_species, "_gene_ensembl"), host = "www.ensembl.org")

  options(ucscChromosomeNames = FALSE)

  # Create a basic axis

  gtrack <- Gviz::GenomeAxisTrack()

  # We should know the start_position and end_position. Fetch a track showing genes in the region

  geneTrack <- Gviz::BiomartGeneRegionTrack(
    chromosome = chromosome, start = start, end = end, name = "Gene", biomart = ensembl, transcriptAnnotation = "symbol",
    collapseTranscripts = TRUE, shape = "arrow"
  )

  # Move gene labels to above

  Gviz::displayPars(geneTrack) <- list(showId = TRUE, fontcolor.title = "black", just.group = "above")

  transcriptTrack <- Gviz::BiomartGeneRegionTrack(chromosome = chromosome, start = start, end = end, name = "Transcripts", biomart = ensembl, transcriptAnnotation = "transcript")

  # Move transcript labels to above

  Gviz::displayPars(transcriptTrack) <- list(showId = TRUE, fontcolor.title = "black", just.group = "above")

  Gviz::plotTracks(list(gtrack, geneTrack, transcriptTrack),
    from = start, to = end, extend.left = 1000, extend.right = 1000, cex = 1, cex.legend = 0.8, cex.group = 0.8,
    cex.title = 0.8
  )
}
