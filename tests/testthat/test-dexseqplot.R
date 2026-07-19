# Happy-path coverage for dexseqplot() with DEXSeq actually installed (a
# Suggests-only dependency; this file is skipped when it's absent). See
# test-dexseqtable.R for why a plain data.frame stands in for a DEXSeqResults
# object here: dexseqplot() only ever passes it straight through to
# DEXSeq::plotDEXSeq(), so mocking that one call (rather than building a real
# DEXSeqResults via DEXSeq's full testForDEU()/estimateExonFoldChanges()
# pipeline) is enough to exercise the module's own reactive logic.

skip_if_not_installed("DEXSeq")

make_dexseqplot_result_table <- function() {
  data.frame(
    groupID = rep(c("gene1", "gene2", "gene3"), each = 2),
    featureID = rep(c("E001", "E002"), 3),
    exonBaseMean = c(120, 80, 200, 150, 60, 40),
    dispersion = c(0.05, 0.06, 0.04, 0.05, 0.07, 0.08),
    stat = c(8.1, 2.1, 7.5, 1.9, 6.2, 1.1),
    pvalue = c(0.0001, 0.2, 0.0005, 0.3, 0.001, 0.4),
    padj = c(0.001, 0.4, 0.003, 0.5, 0.006, 0.6),
    control = c(0.2, 0.8, 0.3, 0.7, 0.5, 0.5),
    treated = c(0.7, 0.3, 0.6, 0.4, 0.2, 0.8),
    log2fold_treated_control = c(1, -1, 2, -2, 0.5, -0.5),
    row.names = paste0("gene", rep(1:3, each = 2), ":", rep(c("E001", "E002"), 3))
  )
}

make_dexseqplot_eselist <- function() {
  # 60 genes so selectmatrix's variance slider (always instantiated, even
  # though dexseqplot hides row selection) has enough rows for its default
  # range; only the first 3 are referenced by the DEXSeq stand-in results.
  n_genes <- 60
  n_samples <- 4

  counts <- matrix(
    stats::rnbinom(n_genes * n_samples, mu = 200, size = 5),
    nrow = n_genes, ncol = n_samples,
    dimnames = list(paste0("gene", 1:n_genes), paste0("s", 1:n_samples))
  )

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    condition = c("control", "control", "treated", "treated")
  )

  annotation <- data.frame(
    gene_id = rownames(counts),
    gene_name = paste0("Gene", 1:n_genes),
    row.names = rownames(counts)
  )

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name",
    dexseq_results = list(make_dexseqplot_result_table())
  )

  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese),
    group_vars = "condition",
    default_groupvar = "condition"
  )
  eselist@contrasts <- list(
    list(Variable = "condition", Group.1 = "control", Group.2 = "treated")
  )
  eselist
}

mock_dexseq_counts <- matrix(
  c(
    10, 12, 40, 42,
    100, 102, 5, 7,
    30, 32, 70, 72,
    60, 62, 40, 42,
    50, 52, 20, 22,
    15, 17, 55, 57
  ),
  nrow = 6, byrow = TRUE,
  dimnames = list(rownames(make_dexseqplot_result_table()), paste0("s", 1:4))
)

run_dexseqplot_server <- function(eselist, extra_inputs = list(), expr) {
  inputs <- modifyList(
    list(
      "deuPlotTable-expression-experiment" = "counts",
      "deuPlotTable-expression-assay" = "counts",
      "deuPlotTable-expression-selectmatrix-sampleSelect" = "all",
      "deuPlotTable-expression-selectmatrix-geneSelect" = "all",
      "deuPlotTable-deuContrast-filterRows" = FALSE,
      "deuPlotTable-deuContrast-contrasts0" = "1",
      "genesymbol-metaField" = "gene_name",
      "genesymbol-label" = "Gene1",
      "deuQvalPlotMax" = 0.1,
      "deuNorcounts" = FALSE,
      "deuSplicing" = TRUE,
      "deuDisplayTranscripts" = TRUE,
      "deuExpression" = TRUE
    ),
    extra_inputs
  )

  testthat::local_mocked_bindings(counts = function(object, normalized = TRUE) mock_dexseq_counts, .package = "DEXSeq")

  shiny::testServer(dexseqplot, args = list(id = "dexseqplot", eselist = eselist), {
    do.call(session$setInputs, inputs)
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

test_that("getSelectedDEUResult returns the DEXSeq results for the selected contrast", {
  run_dexseqplot_server(make_dexseqplot_eselist(), expr = quote({
    result <- getSelectedDEUResult()
    expect_identical(result, make_dexseqplot_result_table())
  }))
})

test_that("getSelectedDEUResult reports a validation message when the selected contrast has no DEU results", {
  eselist <- make_dexseqplot_eselist()

  run_dexseqplot_server(eselist,
    extra_inputs = list("deuPlotTable-deuContrast-contrasts0" = "2"),
    expr = quote({
      err <- tryCatch(getSelectedDEUResult(), error = function(e) e)
      expect_s3_class(err, "validation")
    })
  )
})

test_that("getDEUGeneID resolves a composite exon groupID from the selected gene label", {
  run_dexseqplot_server(make_dexseqplot_eselist(), expr = quote({
    expect_equal(getDEUGeneID(), "gene1")
  }))
})

test_that("dexseqplot draws a DEU plot via DEXSeq::plotDEXSeq with the resolved gene, contrast and control values", {
  recorded_calls <- list()
  testthat::local_mocked_bindings(
    plotDEXSeq = function(object, geneID, FDR, fitExpToVar, norCounts, splicing, displayTranscripts, expression, ...) {
      recorded_calls[[length(recorded_calls) + 1]] <<- list(
        geneID = geneID, FDR = FDR, fitExpToVar = fitExpToVar, norCounts = norCounts,
        splicing = splicing, displayTranscripts = displayTranscripts, expression = expression
      )
      graphics::plot.new()
    },
    .package = "DEXSeq"
  )

  run_dexseqplot_server(make_dexseqplot_eselist(), expr = quote({
    output$deuPlot
  }))

  expect_length(recorded_calls, 1)
  expect_equal(recorded_calls[[1]]$geneID, "gene1")
  expect_equal(recorded_calls[[1]]$FDR, 0.1)
  expect_equal(recorded_calls[[1]]$fitExpToVar, "condition")
  expect_false(recorded_calls[[1]]$norCounts)
  expect_true(recorded_calls[[1]]$splicing)
})

test_that("makeDEUPlotForDownload calls DEXSeq::plotDEXSeq with the same resolved values as the on-screen plot", {
  recorded_calls <- list()
  testthat::local_mocked_bindings(
    plotDEXSeq = function(object, geneID, FDR, fitExpToVar, ...) {
      recorded_calls[[length(recorded_calls) + 1]] <<- list(geneID = geneID, FDR = FDR, fitExpToVar = fitExpToVar)
      graphics::plot.new()
    },
    .package = "DEXSeq"
  )

  run_dexseqplot_server(make_dexseqplot_eselist(), expr = quote({
    makeDEUPlotForDownload()
  }))

  expect_true(length(recorded_calls) >= 1)
  expect_equal(recorded_calls[[length(recorded_calls)]]$geneID, "gene1")
})

test_that("dexseqplot's plot output reports a friendly message when DEXSeq is missing", {
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE)

  run_dexseqplot_server(make_dexseqplot_eselist(), expr = quote({
    err <- tryCatch(output$deuPlot, error = function(e) e)
    expect_s3_class(err, "validation")
    expect_match(conditionMessage(err), "DEXSeq package must be installed")
  }))
})
