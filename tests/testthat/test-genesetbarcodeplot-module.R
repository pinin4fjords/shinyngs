# Module-level testServer coverage for genesetbarcodeplot(). The plain
# exported helper functions in R/genesetbarcodeplot.R (plotly_barcodeplot(),
# tricubeMovingAverage(), quantileOfSorted()) are covered separately in
# test-genesetbarcodeplot.R on a sibling PR branch; this file only exercises
# the Shiny module's own reactive logic, and is named distinctly
# (test-genesetbarcodeplot-module.R) to avoid colliding with that file when
# the two branches are eventually merged.

make_genesetbarcodeplot_eselist <- function() {
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
      SET_B = c("g4", "g5"),
      SET_C = c("g10", "g11")
    ))
  )
  eselist@contrasts <- list(
    list(Variable = "condition", Group.1 = "control", Group.2 = "treated")
  )
  eselist
}

run_genesetbarcodeplot_server <- function(eselist, extra_inputs = list(), expr) {
  inputs <- modifyList(
    list(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all",
      "expression-metafields" = "gene_name",
      "genesetbarcodeplot-filterRows" = FALSE,
      "genesetbarcodeplot-contrasts-summaryType" = "colMeans",
      "genesetbarcodeplot-contrasts0" = "1",
      "genesetbarcodeplot-combine_operator" = "intersect",
      "genesetbarcodeplot-geneSets" = "1-1",
      "genesetbarcodeplot-overlapType" = "union"
    ),
    extra_inputs
  )

  shiny::testServer(genesetbarcodeplot, args = list(id = "genesetbarcodeplot", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    do.call(session$setInputs, inputs)
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

test_that("getContrastsTable/getFoldChanges/getGeneIDs/getLabels resolve the ranking data for all genes", {
  run_genesetbarcodeplot_server(make_genesetbarcodeplot_eselist(), expr = quote({
    ct <- getContrastsTable()
    expect_equal(nrow(ct), 60)

    fc <- getFoldChanges()
    expect_equal(fc[1:6], c(2, 3, -2, -3, 1.5, -1.5))

    ids <- getGeneIDs()
    expect_true(all(c("g1", "g2", "g3", "g4", "g5", "g6") %in% ids))

    labels <- getLabels()
    expect_true(any(grepl("Gene1", labels)))
  }))
})

test_that("barcodeplotTitle includes the gene set name, contrast and resolved enrichment stats", {
  run_genesetbarcodeplot_server(make_genesetbarcodeplot_eselist(), expr = quote({
    title <- barcodeplotTitle()

    expect_match(title, "SET a", fixed = TRUE)
    expect_match(title, "Direction: Up")
    expect_match(title, "FDR: 0.02")
    expect_match(title, "ROAST")
  }))
})

test_that("barcodeplotTitle notes the lack of association when no enrichment result matches", {
  run_genesetbarcodeplot_server(make_genesetbarcodeplot_eselist(),
    extra_inputs = list("genesetbarcodeplot-geneSets" = "1-3"),
    expr = quote({
      title <- barcodeplotTitle()
      expect_match(title, "no association", fixed = TRUE)
    })
  )
})

test_that("genesetbarcodeplot renders a barcode plotly widget for the selected gene set", {
  run_genesetbarcodeplot_server(make_genesetbarcodeplot_eselist(), expr = quote({
    rendered <- output$genesetbarcodeplot
    expect_false(is.null(rendered))

    parsed <- jsonlite::fromJSON(rendered, simplifyVector = FALSE)
    types <- vapply(parsed$x$data, function(t) if (is.null(t$type)) NA_character_ else t$type, character(1))
    expect_true(any(types %in% c("bar", "scatter")))
  }))
})

test_that("gsbpContrastsTable subsets the contrasts table down to the selected gene set's members", {
  run_genesetbarcodeplot_server(make_genesetbarcodeplot_eselist(), expr = quote({
    table <- gsbpContrastsTable()

    expect_equal(nrow(table), 3)
    expect_setequal(table[[prettifyVariablename("gene_name")]], c("Gene1", "Gene2", "Gene3"))
  }))
})

test_that("genesetbarcodeplot renders a simpletable of the gene set's contrast data", {
  run_genesetbarcodeplot_server(make_genesetbarcodeplot_eselist(), expr = quote({
    expect_false(is.null(output[["genesetbarcodeplot-datatable"]]))
  }))
})
