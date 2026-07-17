#' The input function of the illuminaarray module
#'
#' This provides the form elements to control the RNA-seq display
#'
#' The illuminaarray module is a combination of output from many modules (pca, boxplot
#' etc) to form a comprehensive analysis application.
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
#' illuminaarrayInput("illuminaarray", eselist)
#'
illuminaarrayInput <- function(id, eselist) {
  ns <- NS(id)

  navbar_menus <- list(
    id = ns("illuminaarray"),
    title = paste0("Illumina expression array explorer: ", eselist@title),
    window_title = eselist@title,
    homeTab(ns, eselist, "expression array"),
    bslib::nav_menu(
      "Sample data",
      bslib::nav_panel("Experiment",
        value = "Experiment",
        moduleLayout(experimenttableInput(ns("experimenttable"), eselist), experimenttableOutput(ns("experimenttable"))),
        icon = icon("table")
      ),
      bslib::nav_panel("Annotation",
        moduleLayout(rowmetatableInput(ns("rowmetatable"), eselist), rowmetatableOutput(ns("rowmetatable")), width = 260),
        icon = icon("table")
      ),
      icon = icon("flask")
    )
  )

  # Add in the QC/ exploratory menu

  exploratory_menu <- list(
    "QC/ exploratory",
    bslib::nav_panel("Quartile plots",
      moduleLayout(boxplotInput(ns("boxplot"), eselist), boxplotOutput(ns("boxplot"))),
      icon = icon("chart-column", verify_fa = FALSE)
    ),
    bslib::nav_panel("PCA",
      value = "pca",
      moduleLayout(pcaInput(ns("pca"), eselist), pcaOutput(ns("pca"))),
      icon = icon("cube")
    ),
    bslib::nav_panel("PCA vs Experiment",
      moduleLayout(heatmapInput(ns("heatmap-pca"), eselist, type = "pca"), heatmapOutput(ns("heatmap-pca"), type = "pca")),
      icon = icon("cubes")
    ),
    bslib::nav_panel("Clustering dendrogram",
      moduleLayout(dendroInput(ns("dendro"), eselist), dendroOutput(ns("dendro"))),
      icon = icon("sitemap")
    ),
    bslib::nav_panel("Clustering Heatmap",
      moduleLayout(heatmapInput(ns("heatmap-clustering"), eselist, type = "samples"), heatmapOutput(ns("heatmap-clustering"), type = "samples")),
      icon = icon("th", verify_fa = FALSE)
    ),
    bslib::nav_panel("Feature-wise clustering",
      moduleLayout(clusteringInput(ns("feature-clustering"), eselist), clusteringOutput(ns("feature-clustering"))),
      icon = icon("chart-line")
    )
  )

  # Add read reports if provided

  if (any(unlist(lapply(eselist, function(ese) {
    has_slot_data(ese, "read_reports")
  })))) {
    exploratory_menu <- pushToList(exploratory_menu, bslib::nav_panel("Read reports",
      moduleLayout(readreportsInput(ns("readrep"), eselist), readreportsOutput(ns("readrep"))),
      icon = icon("chart-bar", verify_fa = FALSE)
    ))
  }

  # Illumina-specific QC plot

  if ("control" %in% names(eselist)) {
    exploratory_menu <- pushToList(exploratory_menu, bslib::nav_panel("Control probe QC",
      moduleLayout(illuminaarrayqcInput(ns("illuminaarrayqc"), eselist), illuminaarrayqcOutput(ns("illuminaarrayqc"))),
      icon = icon("chart-line")
    ))
  }

  exploratory_menu$icon <- icon("binoculars")

  navbar_menus <- pushToList(navbar_menus, do.call(bslib::nav_menu, exploratory_menu))

  # Add the assay data menu

  assaydata_menu <- list(
    "Assay data",
    bslib::nav_panel("Tables",
      value = "assay_tables",
      moduleLayout(assaydatatableInput(ns("expression"), eselist), assaydatatableOutput(ns("expression"))),
      icon = icon("table")
    ),
    bslib::nav_panel("Heatmaps",
      moduleLayout(heatmapInput(ns("heatmap-expression"), eselist, type = "expression"), heatmapOutput(ns("heatmap-expression"), type = "expression")),
      icon = icon("th", verify_fa = FALSE)
    )
  )

  assaydata_menu$icon <- icon("table")

  navbar_menus <- pushToList(navbar_menus, do.call(bslib::nav_menu, assaydata_menu))

  # If there are contrasts present, add the differential tab

  if (has_slot_data(eselist, "contrasts")) {
    differential_menu <- list(
      "Differential",
      bslib::nav_panel("Tables",
        value = "diff_tables",
        moduleLayout(differentialtableInput(ns("differential"), eselist), differentialtableOutput(ns("differential"))),
        icon = icon("table")
      ),
      bslib::nav_panel("Fold change plots",
        moduleLayout(foldchangeplotInput(ns("foldchange"), eselist), foldchangeplotOutput(ns("foldchange"))),
        icon = icon("chart-line")
      ),
      bslib::nav_panel("MA plots",
        moduleLayout(maplotInput(ns("ma"), eselist), maplotOutput(ns("ma"))),
        icon = icon("chart-line")
      )
    )

    # If any of the experiments in the list have assays with associated contrast_stats, add a volcano plot

    if (any(unlist(lapply(eselist, function(ese) {
      has_slot_data(ese, "contrast_stats")
    })))) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Volcano plots",
        moduleLayout(volcanoplotInput(ns("volcano"), eselist), volcanoplotOutput(ns("volcano"))),
        icon = icon("chart-line")
      ))
    }

    # If any of the experiments have gene set analyses, add this table to the menu

    if (any(unlist(lapply(eselist, function(ese) {
      has_slot_data(ese, "gene_set_analyses")
    })))) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Gene set analyses",
        value = "geneset_analyses",
        moduleLayout(genesetanalysistableInput(ns("genesetanalysis"), eselist), genesetanalysistableOutput(ns("genesetanalysis"))),
        icon = icon("tasks", verify_fa = FALSE)
      ))

      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Gene set barcode plots",
        value = "genesetbarcode",
        moduleLayout(genesetbarcodeplotInput(ns("illuminaarray"), eselist), genesetbarcodeplotOutput(ns("illuminaarray"))),
        icon = icon("barcode")
      ))
    }

    # If any of the experiments have differential exon usage results

    if (any(unlist(lapply(eselist, function(ese) {
      has_slot_data(ese, "dexseq_results")
    })))) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Differential exon usage table",
        moduleLayout(dexseqtableInput(ns("deutable"), eselist), dexseqtableOutput(ns("deutable")))
      ))
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Differential exon usage plot",
        value = "deugene",
        moduleLayout(dexseqplotInput(ns("deuplot"), eselist), dexseqplotOutput(ns("deuplot")))
      ))
    }

    # If there's more than one contrast we can compare differential sets

    if (length(eselist@contrasts) > 1) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Differential set intersection",
        moduleLayout(upsetInput(ns("upset"), eselist), upsetOutput(ns("upset"), eselist)),
        icon = icon("chart-bar", verify_fa = FALSE)
      ))
    }

    differential_menu$icon <- icon("chart-line")

    navbar_menus <- pushToList(navbar_menus, do.call(bslib::nav_menu, differential_menu))
  }

  # Add the gene info plots

  navbar_menus <- pushToList(navbar_menus, bslib::nav_panel("Gene info",
    value = "geneinfo",
    moduleLayout(geneInput(ns("gene"), eselist), geneOutput(ns("gene"), eselist)),
    icon = icon("chart-bar", verify_fa = FALSE)
  ))

  # Add the final wrappers

  shinyngsPageNavbar(navbar_menus)
}

#' The server function of the illuminaarray module
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
#' illuminaarray("illuminaarray", eselist)
#'
illuminaarray <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    # Add internal links to the tables with gene labels

    for (esen in names(eselist)) {
      ese <- eselist[[esen]]

      if (has_slot_data(ese, "labelfield")) {
        eselist@url_roots[[ese@labelfield]] <- "?gene="
        eselist@url_roots$significant_genes <- "?gene="
        eselist@url_roots$gene_set_id <- "?geneset="
      }
    }

    # Now a lot of boring calls to all the modules to activate the UI parts

    summarytiles("summarytiles", eselist)
    experimenttable("experimenttable", eselist)
    rowmetatable("rowmetatable", eselist)
    heatmap("heatmap-clustering", eselist, type = "samples")
    clustering("feature-clustering", eselist)
    illuminaarrayqc("illuminaarrayqc", eselist)
    heatmap("heatmap-expression", eselist, type = "expression")
    heatmap("heatmap-pca", eselist, type = "pca")
    pca("pca", eselist)
    boxplot("boxplot", eselist)
    dendro("dendro", eselist)
    assaydatatable("expression", eselist)

    # Calls for the various optional tables

    if (any(unlist(lapply(eselist, function(ese) {
      has_slot_data(ese, "read_reports")
    })))) {
      readreports("readrep", eselist)
    }

    if (has_slot_data(eselist, "contrasts")) {
      differentialtable("differential", eselist)
      volcanoplot("volcano", eselist)
      foldchangeplot("foldchange", eselist)
      maplot("ma", eselist)
      genesetanalysistable("genesetanalysis", eselist)
      updateBarcodeGeneset <- genesetbarcodeplot("illuminaarray", eselist)
      if (length(eselist@contrasts) > 1) {
        upset("upset", eselist)
      }
    }

    if (any(unlist(lapply(eselist, function(ese) {
      has_slot_data(ese, "dexseq_results")
    })))) {
      dexseqtable("deutable", eselist)
      updateDEUGeneLabel <- dexseqplot("deuplot", eselist)
    }

    updateGeneLabel <- gene("gene", eselist)

    # Catch the specified gene from the URL, switch to the gene info tab, and and use the reactive supplied by the gene module to update its gene label field
    # accordingly

    observe({
      query <- parseQueryString(session$clientData$url_search)

      if (length(intersect(c("gene", "geneset", "deu_gene"), names(query))) == 0) {
        return()
      }

      url_observe <- observe({
        if ("deu_gene" %in% names(query)) {
          updateNavbarPage(session, "illuminaarray", "deugene")
          updateDEUGeneLabel()
        } else if ("gene" %in% names(query)) {
          updateNavbarPage(session, "illuminaarray", "geneinfo")
          updateGeneLabel()
        } else if ("geneset" %in% names(query)) {
          updateNavbarPage(session, "illuminaarray", "genesetbarcode")
          updateBarcodeGeneset()
        }
        url_observe$suspend()
      })
    })
  })
}
