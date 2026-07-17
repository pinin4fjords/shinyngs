# These tests exercise the requireNamespace() guards added around DEXSeq,
# Gviz and biomaRt calls (all Suggests-only, heavy Bioconductor packages).
# None of the three are installed in this test environment, so the guard
# paths below are exercised for real rather than via mocking.

test_that("DEXSeq, Gviz and biomaRt are absent in this test environment", {
  # Documents the precondition the tests below rely on: if any of these ever
  # get pulled in as a transitive dependency of something else, the
  # skip_if()s further down would silently stop testing the guard path.
  expect_false(requireNamespace("DEXSeq", quietly = TRUE))
  expect_false(requireNamespace("Gviz", quietly = TRUE))
  expect_false(requireNamespace("biomaRt", quietly = TRUE))
})

# geneModelPlot() (biomaRt + Gviz)

test_that("geneModelPlot fails with a friendly message when biomaRt/Gviz are missing", {
  skip_if(requireNamespace("biomaRt", quietly = TRUE) && requireNamespace("Gviz", quietly = TRUE))

  result <- tryCatch(
    geneModelPlot(ensembl_species = "mmusculus", chromosome = 1, start = 1, end = 100),
    error = function(e) e
  )

  expect_s3_class(result, "validation")
  expect_match(conditionMessage(result), "biomaRt and Gviz packages must be installed")
})

# dexseqtable() / dexseqplot() (DEXSeq)

dexseq_eselist <- function() {
  n_genes <- 60
  n_samples <- 4

  counts <- matrix(
    stats::rnbinom(n_genes * n_samples, mu = 200, size = 5),
    nrow = n_genes, ncol = n_samples,
    dimnames = list(paste0("gene", seq_len(n_genes)), paste0("sample", seq_len(n_samples)))
  )

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    condition = rep(c("control", "treated"), each = n_samples / 2)
  )

  annotation <- data.frame(
    gene_id = rownames(counts),
    gene_name = paste0("Gene", seq_len(n_genes)),
    row.names = rownames(counts)
  )

  pvals <- matrix(stats::runif(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
  qvals <- matrix(stats::p.adjust(pvals[, 1], method = "BH"), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
  fold_changes <- matrix(stats::rnorm(n_genes, sd = 2), nrow = n_genes, dimnames = list(rownames(counts), "V1"))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name",
    contrast_stats = list(counts = list(pvals = pvals, qvals = qvals, fold_changes = fold_changes)),
    # A placeholder standing in for a list of real DEXSeqResults objects -
    # sufficient to make has_slot_data() report data is present and route
    # this experiment into the dexseq modules, without needing DEXSeq itself.
    dexseq_results = list(list(placeholder = TRUE))
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

test_that("dexseqtable fails with a friendly message when DEXSeq is missing", {
  skip_if(requireNamespace("DEXSeq", quietly = TRUE))

  eselist <- dexseq_eselist()

  shiny::testServer(dexseqtable, args = list(id = "deutable", eselist = eselist), {
    session$setInputs(
      "expression-assay" = "counts",
      "expression-experiment" = "counts"
    )

    result <- tryCatch(makeDEUTables(), error = function(e) e)

    expect_s3_class(result, "validation")
    expect_match(conditionMessage(result), "DEXSeq package must be installed")
  })
})
