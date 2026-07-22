# passing_gene_set_ids()

test_that("passing_gene_set_ids keeps only rows below both thresholds", {
  gst <- data.frame(pvalue = c(0.01, 0.2, 0.03), fdr = c(0.02, 0.3, 0.2), row.names = c("SET_A", "SET_B", "SET_C"))
  col_map <- list(pvalue = "pvalue", fdr = "fdr")

  expect_equal(passing_gene_set_ids(gst, col_map, pval = 0.05, fdr = 0.1), "SET_A")
})

test_that("passing_gene_set_ids returns none when nothing clears the thresholds", {
  gst <- data.frame(pvalue = c(0.2, 0.3), fdr = c(0.3, 0.4), row.names = c("SET_A", "SET_B"))
  col_map <- list(pvalue = "pvalue", fdr = "fdr")

  expect_equal(passing_gene_set_ids(gst, col_map, pval = 0.05, fdr = 0.1), character(0))
})

make_genesetanalysistable_eselist <- function() {
  n_genes <- 60
  n_samples <- 4

  counts <- matrix(
    stats::rnbinom(n_genes * n_samples, mu = 200, size = 5),
    nrow = n_genes, ncol = n_samples,
    dimnames = list(paste0("g", seq_len(n_genes)), paste0("s", seq_len(n_samples)))
  )

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    condition = c("control", "control", "treated", "treated")
  )

  annotation <- data.frame(
    gene_id = rownames(counts),
    gene_name = paste0("Gene", seq_len(n_genes)),
    row.names = rownames(counts)
  )

  fold_changes <- matrix(stats::rnorm(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "1"))
  # g1, g2, g5 up; g3, g4, g6 down - the values genesetselect's "SET_A" test
  # below depends on.
  fold_changes[1:6, 1] <- c(2, 3, -2, -3, 1.5, -1.5)

  gene_set_analyses <- list(counts = list(KEGG = list(
    "condition-control-treated" = data.frame(
      "p value" = c(0.01, 0.2), FDR = c(0.02, 0.3), Direction = c("Up", "Down"),
      row.names = c("SET_A", "SET_B"), check.names = FALSE
    )
  )))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name",
    contrast_stats = list(counts = list(fold_changes = fold_changes)),
    gene_set_analyses = gene_set_analyses
  )

  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese),
    group_vars = "condition",
    default_groupvar = "condition",
    gene_set_id_type = "gene_id",
    gene_sets = list(KEGG = list(
      SET_A = c("g1", "g2", "g3"),
      SET_B = c("g4", "g5")
    ))
  )
  eselist@contrasts <- list(
    list(Variable = "condition", Group.1 = "control", Group.2 = "treated")
  )
  eselist
}

run_genesetanalysistable_server <- function(eselist, extra_inputs = list(), expr) {
  inputs <- modifyList(
    list(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all",
      "genesetanalysistable-filterRows" = FALSE,
      "genesetanalysistable-contrasts-summaryType" = "colMeans",
      "genesetanalysistable-contrasts0" = "1",
      "genesetanalysistable-geneSetTypes" = "KEGG",
      "genesetanalysistable-geneSets" = "1-1",
      "genesetanalysistable-overlapType" = "union",
      "pval" = 0.05,
      "fdr" = 0.1
    ),
    extra_inputs
  )

  shiny::testServer(genesetanalysistable, args = list(id = "genesetanalysistable", eselist = eselist), {
    do.call(session$setInputs, inputs)
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

test_that("getEnrichmentInfo resolves the KEGG enrichment table for the selected contrast", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(), expr = quote({
    enrichment <- getEnrichmentInfo()

    expect_equal(enrichment$tool, "roast")
    expect_equal(rownames(enrichment$gst), c("SET_A", "SET_B"))
  }))
})

test_that("getGeneSetAnalysis filters gene sets by p value and FDR and annotates significant genes", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(), expr = quote({
    gst <- getGeneSetAnalysis()

    expect_equal(nrow(gst), 1)
    expect_equal(gst$gene_set_id, "SET_A")
    expect_equal(gst$significant_genes, "Gene1 Gene2")
  }))
})

test_that("getGeneSetAnalysis reports no results when the p/FDR filters exclude everything", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(),
    extra_inputs = list(pval = 0.001),
    expr = quote({
      err <- tryCatch(getGeneSetAnalysis(), error = function(e) e)
      expect_s3_class(err, "validation")
    })
  )
})

test_that("getFilterablePassingIds only includes gene sets passing the current p/FDR filters", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(), expr = quote({
    expect_equal(getFilterablePassingIds(), "SET_A")
    expect_null(output$geneSetsStatus)
  }))
})

test_that("getFilterablePassingIds is empty when the p/FDR filters exclude everything, and geneSetsStatus reports it", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(),
    extra_inputs = list(pval = 0.001),
    expr = quote({
      expect_equal(getFilterablePassingIds(), character(0))

      err <- tryCatch(output$geneSetsStatus, error = function(e) e)
      expect_s3_class(err, "validation")
      expect_match(conditionMessage(err), "No gene sets meet the current p value/FDR filters")
    })
  )
})

test_that("getGeneSetAnalysis restricts to a specific selected gene set", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(),
    extra_inputs = list(pval = 1, fdr = 1, "genesetanalysistable-geneSets" = "1-2"),
    expr = quote({
      gst <- getGeneSetAnalysis()

      expect_equal(gst$gene_set_id, "SET_B")
      expect_equal(gst$significant_genes, "Gene4")
    })
  )
})

test_that("genesetanalysistable renders a simpletable datatable of the filtered gene sets", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(), expr = quote({
    expect_false(is.null(output[["genesetanalysistable-datatable"]]))
  }))
})

test_that("output$enrichmentMethod reports the resolved enrichment tool", {
  run_genesetanalysistable_server(make_genesetanalysistable_eselist(), expr = quote({
    rendered <- output$enrichmentMethod
    expect_match(rendered[[1]], "ROAST")
  }))
})
