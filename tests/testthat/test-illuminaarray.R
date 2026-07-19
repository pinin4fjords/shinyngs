# illuminaarray() follows the same has_slot_data()-gated composition pattern
# as chipseq() (see test-chipseq.R), plus two illuminaarray-specific gates:
# a dexseq_results slot (adds the DEU table/plot tabs) and a "control"-named
# experiment (adds the Control probe QC tab). As with chipseq, this file
# checks UI composition gating directly and confirms the server boots without
# error, rather than re-driving every nested module's own reactive graph.

make_illuminaarray_eselist <- function(contrasts = FALSE, contrast_stats = FALSE, gene_set_analyses = FALSE,
                                       dexseq_results = FALSE, with_control = FALSE) {
  n_genes <- 20
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

  ese_args <- list(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id", labelfield = "gene_name"
  )

  if (contrast_stats) {
    fold_changes <- matrix(stats::rnorm(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "1"))
    ese_args$contrast_stats <- list(counts = list(fold_changes = fold_changes))
  }

  if (gene_set_analyses) {
    ese_args$gene_set_analyses <- list(counts = list(KEGG = list(
      "condition-control-treated" = data.frame(
        "p value" = c(0.01), FDR = c(0.02), Direction = c("Up"),
        row.names = "SET_A", check.names = FALSE
      )
    )))
  }

  if (dexseq_results) {
    ese_args$dexseq_results <- list(data.frame(
      groupID = c("g1", "g1"), featureID = c("E001", "E002"),
      exonBaseMean = c(10, 20), dispersion = c(0.1, 0.1), stat = c(1, 1),
      pvalue = c(0.01, 0.5), padj = c(0.02, 0.6),
      control = c(0.3, 0.7), treated = c(0.6, 0.4),
      log2fold_treated_control = c(1, -1)
    ))
  }

  ese <- do.call(ExploratorySummarizedExperiment, ese_args)

  eses <- list(counts = ese)

  if (with_control) {
    control_counts <- matrix(
      stats::runif(4 * n_samples, 100, 200),
      nrow = 4, dimnames = list(paste0("ctrl_probe", 1:4), colnames(counts))
    )
    control_annotation <- data.frame(
      probe_id = rownames(control_counts), reporterGroup = rep("housekeeping", 4),
      row.names = rownames(control_counts)
    )
    control_ese <- ExploratorySummarizedExperiment(
      assays = S4Vectors::SimpleList(counts = control_counts), colData = coldata,
      annotation = control_annotation, idfield = "probe_id", labelfield = "probe_id"
    )
    eses$control <- control_ese
  }

  eselist_args <- list(eses = eses, group_vars = "condition", default_groupvar = "condition")
  if (gene_set_analyses) {
    eselist_args$gene_set_id_type <- "gene_id"
    eselist_args$gene_sets <- list(KEGG = list(SET_A = paste0("g", 1:3)))
  }

  eselist <- do.call(ExploratorySummarizedExperimentList, eselist_args)

  if (contrasts) {
    eselist@contrasts <- list(
      list(Variable = "condition", Group.1 = "control", Group.2 = "treated")
    )
  }

  eselist
}

test_that("illuminaarrayInput adds Feature-wise clustering unconditionally", {
  ui <- illuminaarrayInput("illuminaarray", make_illuminaarray_eselist())
  expect_true(grepl("Feature-wise clustering", as.character(ui)))
})

test_that("illuminaarrayInput omits the Control probe QC tab without a 'control' experiment", {
  ui <- illuminaarrayInput("illuminaarray", make_illuminaarray_eselist(with_control = FALSE))
  expect_false(grepl("Control probe QC", as.character(ui)))
})

test_that("illuminaarrayInput adds the Control probe QC tab when a 'control' experiment is present", {
  ui <- illuminaarrayInput("illuminaarray", make_illuminaarray_eselist(with_control = TRUE))
  expect_true(grepl("Control probe QC", as.character(ui)))
})

test_that("illuminaarrayInput omits the DEU tabs without dexseq_results", {
  ui <- illuminaarrayInput("illuminaarray", make_illuminaarray_eselist(contrasts = TRUE, dexseq_results = FALSE))
  expect_false(grepl("Differential exon usage table", as.character(ui)))
})

test_that("illuminaarrayInput adds the DEU table and plot tabs once dexseq_results is present", {
  ui <- illuminaarrayInput("illuminaarray", make_illuminaarray_eselist(contrasts = TRUE, dexseq_results = TRUE))
  txt <- as.character(ui)

  expect_true(grepl("Differential exon usage table", txt))
  expect_true(grepl("Differential exon usage plot", txt))
})

test_that("illuminaarrayInput adds the gene set analysis tabs once gene_set_analyses is present", {
  ui <- illuminaarrayInput("illuminaarray", make_illuminaarray_eselist(contrasts = TRUE, gene_set_analyses = TRUE))
  txt <- as.character(ui)

  expect_true(grepl("Gene set analyses", txt))
  expect_true(grepl("Gene set barcode plots", txt))
})

test_that("illuminaarray boots without error for a minimal eselist", {
  eselist <- make_illuminaarray_eselist()

  expect_no_error(
    withCallingHandlers(
      shiny::testServer(illuminaarray, args = list(id = "illuminaarray", eselist = eselist), {
        session$userData$plotFormat <- function() "png"
        session$elapse(400)
      }),
      warning = function(w) invokeRestart("muffleWarning")
    )
  )
})

test_that("illuminaarray boots without error for a fully-featured eselist (contrasts, contrast_stats, gene_set_analyses, dexseq_results, control probes)", {
  eselist <- make_illuminaarray_eselist(
    contrasts = TRUE, contrast_stats = TRUE, gene_set_analyses = TRUE,
    dexseq_results = TRUE, with_control = TRUE
  )

  expect_no_error(
    withCallingHandlers(
      shiny::testServer(illuminaarray, args = list(id = "illuminaarray", eselist = eselist), {
        session$userData$plotFormat <- function() "png"
        session$elapse(400)
      }),
      warning = function(w) invokeRestart("muffleWarning")
    )
  )
})
