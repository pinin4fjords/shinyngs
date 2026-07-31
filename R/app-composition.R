explorerAppOptions <- function(app_type) {
  options <- switch(app_type,
    rnaseq = list(
      title = "RNA-seq explorer",
      home_label = "RNA-seq",
      distribution_label = "Distribution plots",
      feature_clustering = TRUE,
      array_qc = FALSE,
      dexseq = TRUE,
      dexseq_output_eselist = TRUE
    ),
    chipseq = list(
      title = "ChIP-seq explorer",
      home_label = "ChIP-seq",
      distribution_label = "Quartile plots",
      feature_clustering = FALSE,
      array_qc = FALSE,
      dexseq = FALSE,
      dexseq_output_eselist = FALSE
    ),
    illuminaarray = list(
      title = "Illumina expression array explorer",
      home_label = "expression array",
      distribution_label = "Quartile plots",
      feature_clustering = TRUE,
      array_qc = TRUE,
      dexseq = TRUE,
      dexseq_output_eselist = FALSE
    )
  )

  if (is.null(options)) {
    stop("Unknown explorer app type: ", app_type)
  }

  c(list(app_type = app_type), options)
}

explorerHasExperimentData <- function(eselist, slot_name) {
  any(unlist(lapply(eselist, function(ese) {
    has_slot_data(ese, slot_name)
  })))
}

explorerSampleMenu <- function(ns, eselist) {
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
}

explorerExploratoryMenu <- function(ns, eselist, options) {
  menu <- list(
    "QC/ exploratory",
    bslib::nav_panel(options$distribution_label,
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
    )
  )

  if (options$feature_clustering) {
    menu <- push_to_list(menu, bslib::nav_panel("Feature-wise clustering",
      moduleLayout(clusteringInput(ns("feature-clustering"), eselist), clusteringOutput(ns("feature-clustering"))),
      icon = icon("chart-line")
    ))
  }

  if (explorerHasExperimentData(eselist, "read_reports")) {
    menu <- push_to_list(menu, bslib::nav_panel("Read reports",
      moduleLayout(readreportsInput(ns("readrep"), eselist), readreportsOutput(ns("readrep"))),
      icon = icon("chart-bar", verify_fa = FALSE)
    ))
  }

  if (options$array_qc && "control" %in% names(eselist)) {
    menu <- push_to_list(menu, bslib::nav_panel("Control probe QC",
      moduleLayout(illuminaarrayqcInput(ns("illuminaarrayqc"), eselist), illuminaarrayqcOutput(ns("illuminaarrayqc"))),
      icon = icon("chart-line")
    ))
  }

  menu$icon <- icon("binoculars")
  do.call(bslib::nav_menu, menu)
}

explorerAssayMenu <- function(ns, eselist) {
  bslib::nav_menu(
    "Assay data",
    bslib::nav_panel("Tables",
      value = "assay_tables",
      moduleLayout(assaydatatableInput(ns("expression"), eselist), assaydatatableOutput(ns("expression"))),
      icon = icon("table")
    ),
    bslib::nav_panel("Heatmaps",
      moduleLayout(heatmapInput(ns("heatmap-expression"), eselist, type = "expression"), heatmapOutput(ns("heatmap-expression"), type = "expression")),
      icon = icon("th", verify_fa = FALSE)
    ),
    icon = icon("table")
  )
}

explorerDifferentialMenu <- function(ns, eselist, options) {
  menu <- list(
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

  if (explorerHasExperimentData(eselist, "contrast_stats")) {
    menu <- push_to_list(menu, bslib::nav_panel("Volcano plots",
      moduleLayout(volcanoplotInput(ns("volcano"), eselist), volcanoplotOutput(ns("volcano"))),
      icon = icon("chart-line")
    ))
    menu <- push_to_list(menu, bslib::nav_panel("Top gene boxplots",
      moduleLayout(topgeneboxplotInput(ns("topgeneboxplot"), eselist), topgeneboxplotOutput(ns("topgeneboxplot"))),
      icon = icon("chart-column", verify_fa = FALSE)
    ))
  }

  if (explorerHasExperimentData(eselist, "gene_set_analyses")) {
    menu <- push_to_list(menu, bslib::nav_panel("Gene set analyses",
      value = "geneset_analyses",
      moduleLayout(genesetanalysistableInput(ns("genesetanalysis"), eselist), genesetanalysistableOutput(ns("genesetanalysis"))),
      icon = icon("tasks", verify_fa = FALSE)
    ))
    menu <- push_to_list(menu, bslib::nav_panel("Gene set barcode plots",
      value = "genesetbarcode",
      moduleLayout(genesetbarcodeplotInput(ns(options$app_type), eselist), genesetbarcodeplotOutput(ns(options$app_type))),
      icon = icon("barcode")
    ))
    if (has_cross_contrast_enrichment(eselist)) {
      menu <- push_to_list(menu, bslib::nav_panel("Gene set overview",
        moduleLayout(enrichmentoverviewInput(ns("enrichmentoverview"), eselist), enrichmentoverviewOutput(ns("enrichmentoverview"))),
        icon = icon("chart-line")
      ))
    }
  }

  if (options$dexseq && explorerHasExperimentData(eselist, "dexseq_results")) {
    dexseq_output <- if (options$dexseq_output_eselist) {
      dexseqplotOutput(ns("deuplot"), eselist)
    } else {
      dexseqplotOutput(ns("deuplot"))
    }
    menu <- push_to_list(menu, bslib::nav_panel("Differential exon usage table",
      moduleLayout(dexseqtableInput(ns("deutable"), eselist), dexseqtableOutput(ns("deutable")))
    ))
    menu <- push_to_list(menu, bslib::nav_panel("Differential exon usage plot",
      value = "deugene",
      moduleLayout(dexseqplotInput(ns("deuplot"), eselist), dexseq_output)
    ))
  }

  if (length(eselist@contrasts) > 1) {
    menu <- push_to_list(menu, bslib::nav_panel("Differential set intersection",
      moduleLayout(upsetInput(ns("upset"), eselist), upsetOutput(ns("upset"), eselist)),
      icon = icon("chart-bar", verify_fa = FALSE)
    ))
  }

  menu$icon <- icon("chart-line")
  do.call(bslib::nav_menu, menu)
}

explorerAppInput <- function(id, eselist, app_type) {
  options <- explorerAppOptions(app_type)
  ns <- NS(id)

  menus <- list(
    id = ns(app_type),
    title = paste0(options$title, ": ", eselist@title),
    window_title = eselist@title,
    homeTab(ns, eselist, options$home_label),
    explorerSampleMenu(ns, eselist),
    explorerExploratoryMenu(ns, eselist, options),
    explorerAssayMenu(ns, eselist)
  )

  if (has_slot_data(eselist, "contrasts")) {
    menus <- push_to_list(menus, explorerDifferentialMenu(ns, eselist, options))
  }

  menus <- push_to_list(menus, bslib::nav_panel("Gene info",
    value = "geneinfo",
    moduleLayout(geneInput(ns("gene"), eselist), geneOutput(ns("gene"), eselist)),
    icon = icon("chart-bar", verify_fa = FALSE)
  ))

  shinyngsPageNavbar(menus)
}

explorerUrlRoots <- function(eselist) {
  for (esen in names(eselist)) {
    ese <- eselist[[esen]]
    if (has_slot_data(ese, "labelfield")) {
      eselist@url_roots[[ese@labelfield]] <- "?gene="
      eselist@url_roots$significant_genes <- "?gene="
      eselist@url_roots$gene_set_id <- "?geneset="
    }
  }
  eselist
}

explorerStartModules <- function(eselist, options, heatmap_layout) {
  summarytiles("summarytiles", eselist)
  experimenttable("experimenttable", eselist)
  rowmetatable("rowmetatable", eselist)
  heatmap("heatmap-clustering", eselist, type = "samples", heatmap_layout = heatmap_layout)
  if (options$feature_clustering) {
    clustering("feature-clustering", eselist)
  }
  if (options$array_qc && "control" %in% names(eselist)) {
    illuminaarrayqc("illuminaarrayqc", eselist)
  }
  heatmap("heatmap-expression", eselist, type = "expression", heatmap_layout = heatmap_layout)
  heatmap("heatmap-pca", eselist, type = "pca", heatmap_layout = heatmap_layout)
  pca("pca", eselist)
  boxplot("boxplot", eselist)
  dendro("dendro", eselist)
  assaydatatable("expression", eselist)

  if (explorerHasExperimentData(eselist, "read_reports")) {
    readreports("readrep", eselist)
  }

  callbacks <- list()
  if (has_slot_data(eselist, "contrasts")) {
    differentialtable("differential", eselist)
    foldchangeplot("foldchange", eselist)
    maplot("ma", eselist)
    if (explorerHasExperimentData(eselist, "contrast_stats")) {
      volcanoplot("volcano", eselist)
      topgeneboxplot("topgeneboxplot", eselist)
    }
    if (explorerHasExperimentData(eselist, "gene_set_analyses")) {
      genesetanalysistable("genesetanalysis", eselist)
      callbacks$geneset <- genesetbarcodeplot(options$app_type, eselist)
      if (has_cross_contrast_enrichment(eselist)) {
        enrichmentoverview("enrichmentoverview", eselist)
      }
    }
    if (length(eselist@contrasts) > 1) {
      upset("upset", eselist)
    }
  }

  if (options$dexseq && explorerHasExperimentData(eselist, "dexseq_results")) {
    dexseqtable("deutable", eselist)
    callbacks$deu_gene <- dexseqplot("deuplot", eselist)
  }

  callbacks$gene <- gene("gene", eselist)
  callbacks
}

explorerObserveQuery <- function(session, app_type, callbacks) {
  targets <- list(
    deu_gene = "deugene",
    gene = "geneinfo",
    geneset = "genesetbarcode"
  )
  targets <- targets[names(targets) %in% names(callbacks)]

  observe({
    query <- parseQueryString(session$clientData$url_search)
    selected <- intersect(names(targets), names(query))
    if (length(selected) == 0) {
      return()
    }

    url_observe <- observe({
      query_name <- selected[1]
      updateNavbarPage(session, app_type, targets[[query_name]])
      callbacks[[query_name]]()
      url_observe$suspend()
    })
  })
}

explorerAppServer <- function(id, eselist, app_type, heatmap_layout) {
  options <- explorerAppOptions(app_type)
  moduleServer(id, function(input, output, session) {
    eselist <- explorerUrlRoots(eselist)
    callbacks <- explorerStartModules(eselist, options, heatmap_layout)
    explorerObserveQuery(session, app_type, callbacks)
  })
}
