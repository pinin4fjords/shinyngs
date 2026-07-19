# Happy-path coverage for dexseqtable() with DEXSeq actually installed (it is
# a Suggests-only dependency, so this file is skipped when it's absent; the
# DEXSeq-missing fallback path is already covered in test-optional-heavy-deps.R).
#
# DEXSeqResults objects are S4 DataFrame subclasses normally produced by
# DEXSeq's own testForDEU()/estimateExonFoldChanges() pipeline. Building a real
# one for a test fixture would mean running that whole pipeline. Since
# dexseqtable() only ever touches its DEXSeqResults objects via plain
# data.frame-style indexing (`d$mean1 <-`, `d[, fccol]`, `colnames(d)`,
# `as.data.frame(d[, deucols])`) plus one call to `DEXSeq::counts()`, a plain
# data.frame stand-in plus a mocked `DEXSeq::counts()` reproduces the module's
# real behaviour without needing genuine DEXSeq internals.

skip_if_not_installed("DEXSeq")

make_dexseq_result_table <- function() {
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

make_dexseqtable_eselist <- function() {
  # 60 genes so selectmatrix's (unused, but always instantiated) variance
  # slider has enough rows for its default range; only the first 3 are
  # referenced by the DEXSeq stand-in results below.
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

  pvals <- matrix(stats::runif(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
  qvals <- matrix(stats::p.adjust(pvals[, 1], method = "BH"), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
  fold_changes <- matrix(stats::rnorm(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "V1"))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name",
    contrast_stats = list(counts = list(pvals = pvals, qvals = qvals, fold_changes = fold_changes)),
    dexseq_results = list(make_dexseq_result_table())
  )

  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese),
    group_vars = "condition",
    default_groupvar = "condition"
  )
  eselist@contrasts <- list(
    list(id = "condition_control_treated", Variable = "condition", Group.1 = "control", Group.2 = "treated")
  )
  eselist
}

# A fixed mock counts() matrix, same row order as make_dexseq_result_table(),
# columns matching make_dexseqtable_eselist()'s sample names.
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
  dimnames = list(rownames(make_dexseq_result_table()), paste0("s", 1:4))
)

run_dexseqtable_server <- function(eselist, extra_inputs = list(), args = list(), expr) {
  inputs <- modifyList(
    list(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all",
      "deuContrast-filterRows" = TRUE,
      "deuContrast-contrasts0" = "1",
      "deuContrast-fold_change_card0" = ">= or <= -",
      "deuContrast-fold_change0" = 0,
      "deuContrast-p_value_card0" = "<=",
      "deuContrast-p_value0" = 1,
      "deuContrast-q_value_card0" = "<=",
      "deuContrast-q_value0" = 1,
      "deuMostSigExon" = FALSE
    ),
    extra_inputs
  )

  testthat::local_mocked_bindings(counts = function(object, normalized = TRUE) mock_dexseq_counts, .package = "DEXSeq")

  server_args <- modifyList(list(id = "deutable", eselist = eselist), args)

  shiny::testServer(dexseqtable, args = server_args, {
    do.call(session$setInputs, inputs)
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

test_that("makeDEUTables computes a DEU table per contrast from the DEXSeq results", {
  run_dexseqtable_server(make_dexseqtable_eselist(), expr = quote({
    tables <- makeDEUTables()

    expect_length(tables, 1)
    expect_equal(nrow(tables[[1]]), 6)
    expect_true(all(c("groupID", "Exon", "Relative exon usage fold change", "P value", "FDR corrected p value") %in% colnames(tables[[1]])))
  }))
})

test_that("makeDEUTables converts log2 fold changes to signed linear fold changes", {
  run_dexseqtable_server(make_dexseqtable_eselist(), expr = quote({
    table <- makeDEUTables()[[1]]
    ordered_by_exon <- table[order(table$groupID, table$Exon), ]

    expect_equal(
      ordered_by_exon[["Relative exon usage fold change"]],
      round(c(2, -2, 4, -4, 2^0.5, -1 / 2^-0.5), 2)
    )
  }))
})

test_that("makeDEUTable orders results by FDR corrected p value", {
  run_dexseqtable_server(make_dexseqtable_eselist(), expr = quote({
    table <- makeDEUTable()

    expect_equal(table[["FDR corrected p value"]], sort(table[["FDR corrected p value"]]))
    expect_equal(nrow(table), 6)
  }))
})

test_that("makeDEUTable keeps only the most significant exon per gene when deuMostSigExon is TRUE", {
  run_dexseqtable_server(make_dexseqtable_eselist(),
    extra_inputs = list("deuMostSigExon" = TRUE),
    expr = quote({
      table <- makeDEUTable()

      expect_equal(nrow(table), 3)
      expect_equal(sort(unique(table[[prettify_variable_name("gene_id")]])), c("gene1", "gene2", "gene3"))
    })
  )
})

test_that("makeDEUTable applies fold change and p/q value cardinality filters", {
  run_dexseqtable_server(make_dexseqtable_eselist(),
    extra_inputs = list("deuContrast-p_value0" = 0.01),
    expr = quote({
      table <- makeDEUTable()

      expect_true(all(table[["P value"]] <= 0.01))
      expect_true(nrow(table) < 6)
    })
  )
})

test_that("dexseqtable renders a simpletable datatable from the DEU results", {
  run_dexseqtable_server(make_dexseqtable_eselist(), expr = quote({
    expect_false(is.null(output[["dexseqtable-datatable"]]))
  }))
})

test_that("dexseqtable's getSelectedContrastNumbers reflects the selected contrast", {
  run_dexseqtable_server(make_dexseqtable_eselist(), expr = quote({
    expect_equal(contrast_reactives$getSelectedContrastNumbers()[[1]][[1]], "1")
  }))
})
