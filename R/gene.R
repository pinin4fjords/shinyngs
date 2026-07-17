gene_modal <- list(id = "gene", title = "Gene information")

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

  moduleMain(
    NULL,
    uiOutput(ns("model")),
    uiOutput(ns("info")),
    uiOutput(ns("title")),
    plotlyOutput(ns("barPlot"), height = 500),
    h4("Contrasts table"),
    simpletableOutput(ns("geneContrastsTable")),
    help = modalInput(ns(gene_modal$id), "help", "help")
  )
}

#' The server function of the gene module
#'
#' The gene module picks specified rows out the assay data, either simply by id
#' or label. This is used to create a gene-centric info page.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' gene("gene", eselist)
#'
gene <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(gene_modal$id, gene_modal$title)

    # Call all the required modules and hold on to their reactives

    selectmatrix_reactives <- selectmatrix("gene", eselist, var_n = 1000, select_samples = TRUE, select_genes = FALSE, provide_all_genes = FALSE)
    gene_label_reactives <- labelselectfield("gene_label",
      eselist = eselist, getExperiment = selectmatrix_reactives$getExperiment, labels_from_all_experiments = TRUE,
      url_field = "gene", id_selection = TRUE, getNonEmptyRows = selectmatrix_reactives$getNonEmptyRows
    )
    contrast_reactives <- contrasts("gene", eselist = eselist, multiple = TRUE, show_controls = FALSE, selectmatrix_reactives = selectmatrix_reactives, select_all_contrasts = TRUE)
    groupby_reactives <- groupby("gene", eselist = eselist, group_label = "Color by", selectColData = selectmatrix_reactives$selectColData)

    # Help modals are shown from the reactive info/model links, with titles
    # that track the currently selected experiment and gene

    gene_info_modal_id <- "geneInfo"
    gene_model_modal_id <- "geneModel"

    modalServer(gene_info_modal_id,
      title = function() paste(selectmatrix_reactives$getExperimentName(), "information for", paste(gene_label_reactives$getSelectedLabels(), sep = ", ")),
      content = DT::dataTableOutput(session$ns("geneInfoTable"))
    )

    modalServer(gene_model_modal_id,
      title = function() paste(gene_label_reactives$getSelectedLabels()[1], "gene model"),
      content = igvShiny::igvShinyOutput(session$ns("geneModel"), height = "600px")
    )

    # The title and info link are reactive to the currently active experiment

    output$title <- renderUI({
      en <- selectmatrix_reactives$getExperimentName()
      h3(paste(en, "expression bar plot"))
    })

    output$info <- renderUI({
      ns <- session$ns
      en <- selectmatrix_reactives$getExperimentName()

      list(modalInput(ns(gene_info_modal_id), paste(en, " info"), "help"))
    })

    # Render the gene model plot

    output$model <- renderUI({
      ns <- session$ns

      if (has_slot_data(eselist, "ensembl_species")) {
        out <- list(modalInput(ns(gene_model_modal_id), "Gene model", "help"))
      }
    })

    # Get the rows with valid data. Some rows of some assays may be blank

    getSelectedIdsWithData <- reactive({
      rowids <- gene_label_reactives$getSelectedIds()

      sm <- selectmatrix_reactives$selectMatrix()
      rowids <- rowids[rowids %in% rownames(sm)]
      gene_labels <- gene_label_reactives$getSelectedLabels()
      assay <- selectmatrix_reactives$getAssay()

      validate(need(length(rowids) > 0, paste0("No values for gene labels '", paste(gene_labels, collapse = "', '"), "' in assay '", assay, "'")))

      rowids
    })

    # Render the bar plot with plotly

    output$barPlot <- renderPlotly({
      withProgress(message = "Making bar plot", value = 0, {
        ese <- selectmatrix_reactives$getExperiment()
        rows <- getSelectedIdsWithData()
        barplot_expression <- selectmatrix_reactives$selectMatrix()
        barplot_expression <- barplot_expression[rows, , drop = FALSE]

        if (has_slot_data(ese, "labelfield")) {
          rownames(barplot_expression) <- idToLabel(rows, ese, sep = "<br />")
        }

        coldata <- selectmatrix_reactives$selectColData()
        groupby <- groupby_reactives$getGroupby()
        assaymeasure <- selectmatrix_reactives$getAssayMeasure()
        palette <- groupby_reactives$getPalette()

        p <- geneBarplot(barplot_expression, coldata, groupby, assaymeasure, palette = palette)
      })
    })

    # Make a gene region plot with igvShiny (a real, interactive genome
    # browser widget, rather than static Gviz graphics)

    getGeneModelLocus <- reactive({
      gene_labels <- gene_label_reactives$getSelectedLabels()
      ese <- selectmatrix_reactives$getExperiment()

      annotation <- data.frame(SummarizedExperiment::mcols(ese))
      annotation <- annotation[which(annotation[[ese@labelfield]] == gene_labels[1]), ]

      list(
        chromosome = annotation$chromosome_name[1],
        start = min(annotation$start_position),
        end = max(annotation$end_position)
      )
    })

    output$geneModel <- igvShiny::renderIgvShiny({
      validate(need(requireNamespace("igvShiny", quietly = TRUE), "The igvShiny package must be installed to view gene model plots."))

      locus <- getGeneModelLocus()
      genome_info <- geneModelGenomeInfo(eselist@ensembl_species)

      validate(need(!is.null(genome_info), paste0("No igv.js genome build is known for species '", eselist@ensembl_species, "'")))

      genomeOptions <- igvShiny::parseAndValidateGenomeSpec(
        genomeName = genome_info$genome,
        initialLocus = paste0("chr", locus$chromosome, ":", locus$start, "-", locus$end)
      )
      igvShiny::igvShiny(genomeOptions, displayMode = "EXPANDED")
    })

    # Once the widget signals it's ready, layer the full Ensembl transcript
    # catalog (colored by biotype) on top of the browser's own bundled RefSeq
    # track, for genome builds where a hosted, tabix-indexed Ensembl GFF3 is
    # known (see geneModelGenomeInfo() -- currently hg38 only; other builds
    # fall back to the widget's default annotation track).

    observeEvent(input$igvReady, {
      locus <- getGeneModelLocus()
      genome_info <- geneModelGenomeInfo(eselist@ensembl_species)

      if (!is.null(genome_info) && !is.null(genome_info$gff3_url)) {
        igvShiny::loadGFF3TrackFromURL(session,
          id = session$ns("geneModel"), trackName = "Ensembl transcripts (biotype)",
          gff3URL = genome_info$gff3_url, indexURL = genome_info$gff3_index_url,
          color = "black", colorTable = geneModelBiotypeColors(), colorByAttribute = "biotype",
          displayMode = "EXPANDED", trackHeight = 700, visibilityWindow = 100000,
          deleteTracksOfSameName = TRUE
        )
      }
    })

    # Make a table of the annotation data

    output$geneInfoTable <- DT::renderDataTable(
      {
        rows <- gene_label_reactives$getSelectedIds()
        ese <- selectmatrix_reactives$getExperiment()

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
      contrasts_table <- contrast_reactives$labelledContrastsTable()
      id_field <- selectmatrix_reactives$getIdField()

      contrasts_table[contrasts_table[[prettifyVariablename(id_field)]] %in% rows, , drop = FALSE]
    })

    # Link the contrasts table for display

    getLinkedGeneContrastsTable <- reactive({
      gene_contrasts_table <- getGeneContrastsTable()
      linkMatrix(gene_contrasts_table, url_roots = eselist@url_roots)
    })

    # Render the contrasts table- when a valid label is supplied

    simpletable("geneContrastsTable", downloadMatrix = getGeneContrastsTable, displayMatrix = getLinkedGeneContrastsTable, filename = "gene_contrasts", rownames = FALSE)

    # Return the reactive for updating the gene input field. Will be used for updating the field when linking to this panel

    gene_label_reactives$updateLabelField
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
#' @param palette Palette of colors, one for each unique value derived from
#' \code{colorby}.
#' @param expressionmeasure String to use for labelling y axis in plots
#'
#' @keywords shiny
#'
#' @examples
#' gene("gene", ses)
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

  plots <- lapply(seq_len(nrow(expression)), function(rowno) {
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
  p
}


#' Map an Ensembl species name to an igv.js genome build and, where known, a
#' hosted Ensembl GFF3 annotation for that build
#'
#' igv.js ships a handful of stock genomes with their own default annotation
#' track (RefSeq), which only shows canonical/collapsed transcripts. Where a
#' hosted, tabix-indexed Ensembl GFF3 is publicly available for that build, we
#' additionally load it as a second track to show the full transcript
#' catalog, matching what the old Gviz-based plot displayed.
#'
#' @param ensembl_species Ensembl species definition like 'hsapiens'
#'
#' @return A list with \code{genome} (an igv.js genome build name) and,
#' optionally, \code{gff3_url}/\code{gff3_index_url}; or \code{NULL} if no
#' igv.js build is known for \code{ensembl_species}.

geneModelGenomeInfo <- function(ensembl_species) {
  genome_map <- list(
    hsapiens = list(
      genome = "hg38",
      gff3_url = "https://s3.amazonaws.com/igv.org.genomes/hg38/Homo_sapiens.GRCh38.94.chr.gff3.gz",
      gff3_index_url = "https://s3.amazonaws.com/igv.org.genomes/hg38/Homo_sapiens.GRCh38.94.chr.gff3.gz.tbi"
    ),
    mmusculus = list(genome = "mm10"),
    drerio = list(genome = "danRer11"),
    scerevisiae = list(genome = "sacCer3"),
    dmelanogaster = list(genome = "dm6")
  )

  genome_map[[ensembl_species]]
}

#' Default color table for Ensembl transcript biotypes, used to color the
#' full transcript catalog track loaded by the \code{gene} module's gene
#' model view
#'
#' @return A named list mapping biotype strings to colors, plus a "default"
#' fallback

geneModelBiotypeColors <- function() {
  list(
    protein_coding = "#E69F00",
    nonsense_mediated_decay = "#56B4E9",
    retained_intron = "#999999",
    processed_transcript = "#CC79A7",
    default = "black"
  )
}
