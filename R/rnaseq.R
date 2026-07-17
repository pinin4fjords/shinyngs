#' The input function of the rnaseq module
#'
#' This provides the form elements to control the RNA-seq display
#'
#' The rnaseq module is a combination of output from many modules (pca, boxplot
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
#' rnaseqInput("rnaseq", eselist)
#'
rnaseqInput <- function(id, eselist) {
  ns <- NS(id)

  # Build menu structure based on available information

  navbar_menus <- list(id = ns("rnaseq"), title = paste0("RNA-seq explorer: ", eselist@title), window_title = eselist@title, homeTab(ns, eselist, "RNA-seq"), bslib::nav_menu("Sample data", bslib::nav_panel("Experiment", value = "Experiment", sidebarLayout(sidebarPanel(experimenttableInput(
    ns("experimenttable"),
    eselist
  ), width = 3), mainPanel(experimenttableOutput(ns("experimenttable")), width = 9)), icon = icon("table")), bslib::nav_panel("Annotation", sidebarLayout(sidebarPanel(rowmetatableInput(
    ns("rowmetatable"),
    eselist
  ), width = 2), mainPanel(rowmetatableOutput(ns("rowmetatable")), width = 10)), icon = icon("table")), icon = icon("flask")))

  # Add in the QC/ exploratory menu

  exploratory_menu <- list("QC/ exploratory", bslib::nav_panel("Distribution plots", sidebarLayout(sidebarPanel(boxplotInput(ns("boxplot"), eselist), width = 3), mainPanel(boxplotOutput(ns("boxplot")),
    width = 9
  )), icon = icon("chart-column", verify_fa = FALSE)), bslib::nav_panel("PCA", value = "pca", sidebarLayout(sidebarPanel(pcaInput(ns("pca"), eselist), width = 3), mainPanel(pcaOutput(ns("pca")),
    width = 9
  )), icon = icon("cube")), bslib::nav_panel("PCA vs Experiment", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-pca"), eselist, type = "pca"),
    width = 3
  ), mainPanel(heatmapOutput(ns("heatmap-pca"), type = "pca"), width = 9)), icon = icon("cubes")), bslib::nav_panel("Clustering dendrogram", sidebarLayout(sidebarPanel(dendroInput(
    ns("dendro"),
    eselist
  ), width = 3), mainPanel(dendroOutput(ns("dendro")), width = 9)), icon = icon("sitemap")), bslib::nav_panel("Clustering Heatmap", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-clustering"),
    eselist,
    type = "samples"
  ), width = 3), mainPanel(heatmapOutput(ns("heatmap-clustering"), type = "samples"), width = 9)), icon = icon("th", verify_fa = FALSE)), bslib::nav_panel("Feature-wise clustering",
    sidebarLayout(sidebarPanel(clusteringInput(ns("feature-clustering"), eselist), width = 3), mainPanel(clusteringOutput(ns("feature-clustering")), width = 9)),
    icon = icon("chart-line")
  ))

  # Add read reports if provided

  if (any(unlist(lapply(eselist, function(ese) {
    length(ese@read_reports) > 0
  })))) {
    exploratory_menu <- pushToList(exploratory_menu, bslib::nav_panel("Read reports", sidebarLayout(
      sidebarPanel(readreportsInput(ns("readrep"), eselist), width = 3),
      mainPanel(readreportsOutput(ns("readrep")), width = 9)
    ), icon = icon("chart-bar", verify_fa = FALSE)))
  }
  exploratory_menu$icon <- icon("binoculars")

  navbar_menus <- pushToList(navbar_menus, do.call(bslib::nav_menu, exploratory_menu))

  #  # Add the assay data menu

  assaydata_menu <- list("Assay data", bslib::nav_panel("Tables", value = "assay_tables", sidebarLayout(sidebarPanel(assaydatatableInput(ns("expression"), eselist), width = 3), mainPanel(assaydatatableOutput(ns("expression")),
    width = 9
  )), icon = icon("table")), bslib::nav_panel("Heatmaps", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-expression"), eselist, type = "expression"),
    width = 3
  ), mainPanel(heatmapOutput(ns("heatmap-expression"), type = "expression"), width = 9)), icon = icon("th", verify_fa = FALSE)))

  assaydata_menu$icon <- icon("table")

  navbar_menus <- pushToList(navbar_menus, do.call(bslib::nav_menu, assaydata_menu))

  # If there are contrasts present, add the differential tab

  if (length(eselist@contrasts) > 0) {
    differential_menu <- list("Differential", bslib::nav_panel("Tables", value = "diff_tables", sidebarLayout(
      sidebarPanel(differentialtableInput(ns("differential"), eselist), width = 3),
      mainPanel(differentialtableOutput(ns("differential")), width = 9)
    ), icon = icon("table")), bslib::nav_panel("Fold change plots", sidebarLayout(sidebarPanel(foldchangeplotInput(
      ns("foldchange"),
      eselist
    ), width = 3), mainPanel(foldchangeplotOutput(ns("foldchange")), width = 9)), icon = icon("chart-line")), bslib::nav_panel("MA plots", sidebarLayout(sidebarPanel(maplotInput(
      ns("ma"),
      eselist
    ), width = 3), mainPanel(maplotOutput(ns("ma")), width = 9)), icon = icon("chart-line")))

    # If any of the experiments in the list have assays with associated contrast_stats, add a volcano plot

    if (any(unlist(lapply(eselist, function(ese) {
      length(ese@contrast_stats) > 0
    })))) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Volcano plots", sidebarLayout(sidebarPanel(volcanoplotInput(ns("volcano"), eselist),
        width = 3
      ), mainPanel(volcanoplotOutput(ns("volcano")), width = 9)), icon = icon("chart-line")))
    }

    # If any of the experiments have gene set analyses, add this table to the menu

    if (any(unlist(lapply(eselist, function(ese) {
      length(ese@gene_set_analyses) > 0
    })))) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Gene set analyses", value = "geneset_analyses", sidebarLayout(sidebarPanel(genesetanalysistableInput(
        ns("genesetanalysis"),
        eselist
      ), width = 3), mainPanel(genesetanalysistableOutput(ns("genesetanalysis")), width = 9)), icon = icon("tasks", verify_fa = FALSE)))

      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Gene set barcode plots", value = "genesetbarcode", sidebarLayout(sidebarPanel(genesetbarcodeplotInput(
        ns("rnaseq"),
        eselist
      ), width = 3), mainPanel(genesetbarcodeplotOutput(ns("rnaseq")), width = 9)), icon = icon("barcode")))
    }

    # If any of the experiments have differential exon usage results

    if (any(unlist(lapply(eselist, function(ese) {
      length(ese@dexseq_results) > 0
    })))) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Differential exon usage table", sidebarLayout(sidebarPanel(dexseqtableInput(
        ns("deutable"),
        eselist
      ), width = 3), mainPanel(dexseqtableOutput(ns("deutable")), width = 9))))
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Differential exon usage plot", value = "deugene", sidebarLayout(sidebarPanel(dexseqplotInput(
        ns("deuplot"),
        eselist
      ), width = 3), mainPanel(dexseqplotOutput(ns("deuplot")), width = 9))))
    }

    # If there's more than one contrast we can compare differential sets

    if (length(eselist@contrasts) > 1) {
      differential_menu <- pushToList(differential_menu, bslib::nav_panel("Differential set intersection", sidebarLayout(sidebarPanel(upsetInput(
        ns("upset"),
        eselist
      ), width = 3), mainPanel(upsetOutput(ns("upset"), eselist), width = 9)), icon = icon("chart-bar", verify_fa = FALSE)))
    }

    differential_menu$icon <- icon("chart-line")

    navbar_menus <- pushToList(navbar_menus, do.call(bslib::nav_menu, differential_menu))
  }

  # Add the gene info plots

  navbar_menus <- pushToList(navbar_menus, bslib::nav_panel("Gene info", value = "geneinfo", sidebarLayout(
    sidebarPanel(geneInput(ns("gene"), eselist), width = 3),
    mainPanel(geneOutput(ns("gene"), eselist), width = 9)
  ), icon = icon("chart-bar", verify_fa = FALSE)))

  # Add the final wrappers

  shinyngsPageNavbar(navbar_menus)
}

#' The server function of the rnaseq module
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
#' rnaseq("rnaseq", eselist)
#'
rnaseq <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    # Add internal links to the tables with gene labels

    for (esen in names(eselist)) {
      ese <- eselist[[esen]]

      if (length(ese@labelfield) > 0) {
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
    heatmap("heatmap-expression", eselist, type = "expression")
    heatmap("heatmap-pca", eselist, type = "pca")
    pca("pca", eselist)
    boxplot("boxplot", eselist)
    dendro("dendro", eselist)
    assaydatatable("expression", eselist)

    # Calls for the various optional tables

    if (any(unlist(lapply(eselist, function(ese) {
      length(ese@read_reports) > 0
    })))) {
      readreports("readrep", eselist)
    }

    if (length(eselist@contrasts) > 0) {
      differentialtable("differential", eselist)
      volcanoplot("volcano", eselist)
      foldchangeplot("foldchange", eselist)
      maplot("ma", eselist)
      genesetanalysistable("genesetanalysis", eselist)
      updateBarcodeGeneset <- genesetbarcodeplot("rnaseq", eselist)
      if (length(eselist@contrasts) > 1) {
        upset("upset", eselist)
      }
    }

    if (any(unlist(lapply(eselist, function(ese) {
      length(ese@dexseq_results) > 0
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
          updateNavbarPage(session, "rnaseq", "deugene")
          updateDEUGeneLabel()
        } else if ("gene" %in% names(query)) {
          updateNavbarPage(session, "rnaseq", "geneinfo")
          updateGeneLabel()
        } else if ("geneset" %in% names(query)) {
          updateNavbarPage(session, "rnaseq", "genesetbarcode")
          updateBarcodeGeneset()
        }
        url_observe$suspend()
      })
    })
  })
}
