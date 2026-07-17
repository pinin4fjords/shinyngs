# These tests exercise the requireNamespace() guards added around DEXSeq
# and igvShiny calls (all Suggests-only, heavy dependencies). requireNamespace()
# is mocked to report the package as absent, so the guard path is exercised
# deterministically regardless of whether these packages actually happen to
# be installed in the environment running the tests.

# gene() module's gene model view (igvShiny)

gene_model_eselist <- function() {
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
    chromosome_name = c("17", rep(NA, n_genes - 1)),
    start_position = c(7661779, rep(NA, n_genes - 1)),
    end_position = c(7687546, rep(NA, n_genes - 1)),
    row.names = rownames(counts)
  )

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name"
  )

  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese),
    group_vars = "condition",
    default_groupvar = "condition",
    ensembl_species = "hsapiens"
  )
  eselist@contrasts <- list(
    list(id = "condition_control_treated", Variable = "condition", Group.1 = "control", Group.2 = "treated")
  )
  eselist
}

test_that("gene model view falls back to a friendly message when igvShiny is missing", {
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE)

  eselist <- gene_model_eselist()

  shiny::testServer(gene, args = list(id = "gene", eselist = eselist), {
    session$setInputs(
      "gene-experiment" = "counts",
      "gene-assay" = "counts",
      "gene_label-label" = "Gene1"
    )

    # output$geneModel is never registered at all in this case -- gene()
    # only assigns it inside its own requireNamespace("igvShiny") guard, so
    # the modal content falls back to a plain message instead, which is what
    # a user actually sees.
    expect_match(as.character(gene_model_content), "igvShiny package must be installed")
  })
})

# geneModelGenomeInfo() is a plain function (no Shiny/igvShiny involved), so
# this is tested directly rather than through the full module -- the "no
# genome build known" validate() message in output$geneModel is only
# reachable when igvShiny is actually installed, which it may not be in a
# CI environment that (deliberately) doesn't install Suggests-only heavy deps.

test_that("geneModelGenomeInfo returns NULL for an unrecognised species", {
  expect_null(geneModelGenomeInfo("not_a_real_species"))
})

test_that("geneModelGenomeInfo returns a genome build for known species", {
  info <- geneModelGenomeInfo("hsapiens")
  expect_equal(info$genome, "hg38")
  expect_match(info$gff3_url, "^https://")

  expect_equal(geneModelGenomeInfo("mmusculus")$genome, "mm10")
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
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE)

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
