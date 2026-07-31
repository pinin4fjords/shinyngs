# chipseq() composes many already-tested modules behind has_slot_data() gates
# (read_reports, contrasts, contrast_stats, gene_set_analyses). Rather than
# re-driving every nested module's own reactive graph (covered by their own
# dedicated test files), this file checks: (1) the UI composition gating logic
# by calling chipseqInput() directly against fixtures with/without each
# optional slot and inspecting the rendered tab structure, and (2) that the
# server function boots without error for both a minimal and a fully-featured
# eselist, exercising each has_slot_data()-gated submodule call at least once.

make_chipseq_eselist <- function(contrasts = FALSE, contrast_stats = FALSE, read_reports = FALSE, gene_set_analyses = FALSE) {
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

  if (read_reports) {
    ese_args$read_reports <- list(counts = matrix(
      stats::runif(4 * n_samples),
      nrow = 4, dimnames = list(paste0("metric", 1:4), colnames(counts))
    ))
  }

  if (gene_set_analyses) {
    ese_args$gene_set_analyses <- list(counts = list(KEGG = list(
      "condition-control-treated" = data.frame(
        "p value" = c(0.01), FDR = c(0.02), Direction = c("Up"),
        row.names = "SET_A", check.names = FALSE
      )
    )))
  }

  ese <- do.call(ExploratorySummarizedExperiment, ese_args)

  eselist_args <- list(
    eses = list(counts = ese), group_vars = "condition", default_groupvar = "condition"
  )
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

test_that("chipseqInput omits the Differential menu entirely when there are no contrasts", {
  ui <- chipseqInput("chipseq", make_chipseq_eselist(contrasts = FALSE))
  txt <- as.character(ui)

  expect_false(grepl("Differential set intersection", txt))
  expect_false(grepl("Fold change plots", txt))
})

test_that("chipseqInput includes the Differential menu's core tabs once contrasts are present", {
  ui <- chipseqInput("chipseq", make_chipseq_eselist(contrasts = TRUE))
  txt <- as.character(ui)

  expect_true(grepl("Fold change plots", txt))
  expect_true(grepl("MA plots", txt))
  expect_false(grepl("Volcano plots", txt))
  expect_false(grepl("Gene set analyses", txt))
})

test_that("chipseqInput adds Volcano plots and Top gene boxplots once contrast_stats is present", {
  ui <- chipseqInput("chipseq", make_chipseq_eselist(contrasts = TRUE, contrast_stats = TRUE))
  txt <- as.character(ui)

  expect_true(grepl("Volcano plots", txt))
  expect_true(grepl("Top gene boxplots", txt))
})

test_that("chipseqInput adds the gene set analysis tabs once gene_set_analyses is present", {
  ui <- chipseqInput("chipseq", make_chipseq_eselist(contrasts = TRUE, gene_set_analyses = TRUE))
  txt <- as.character(ui)

  expect_true(grepl("Gene set analyses", txt))
  expect_true(grepl("Gene set barcode plots", txt))
})

test_that("chipseqInput adds a Read reports tab only when read_reports is present", {
  without_reports <- as.character(chipseqInput("chipseq", make_chipseq_eselist()))
  with_reports <- as.character(chipseqInput("chipseq", make_chipseq_eselist(read_reports = TRUE)))

  expect_false(grepl("Read reports", without_reports))
  expect_true(grepl("Read reports", with_reports))
})

test_that("chipseqInput only adds Differential set intersection with more than one contrast", {
  eselist_one <- make_chipseq_eselist(contrasts = TRUE)
  eselist_two <- make_chipseq_eselist(contrasts = TRUE)
  eselist_two@contrasts <- list(
    list(Variable = "condition", Group.1 = "control", Group.2 = "treated"),
    list(Variable = "condition", Group.1 = "treated", Group.2 = "control")
  )

  expect_false(grepl("Differential set intersection", as.character(chipseqInput("chipseq", eselist_one))))
  expect_true(grepl("Differential set intersection", as.character(chipseqInput("chipseq", eselist_two))))
})

test_that("chipseq boots without error for a minimal eselist (no contrasts, no optional slots)", {
  eselist <- make_chipseq_eselist()

  expect_no_error(
    withCallingHandlers(
      shiny::testServer(chipseq, args = list(id = "chipseq", eselist = eselist), {
        session$userData$plotFormat <- function() "png"
        session$elapse(400)
      }),
      warning = function(w) invokeRestart("muffleWarning")
    )
  )
})

test_that("chipseq boots without error for a fully-featured eselist (contrasts, contrast_stats, read_reports, gene_set_analyses)", {
  eselist <- make_chipseq_eselist(contrasts = TRUE, contrast_stats = TRUE, read_reports = TRUE, gene_set_analyses = TRUE)

  expect_no_error(
    withCallingHandlers(
      shiny::testServer(chipseq, args = list(id = "chipseq", eselist = eselist), {
        session$userData$plotFormat <- function() "png"
        session$elapse(400)
      }),
      warning = function(w) invokeRestart("muffleWarning")
    )
  )
})

test_that("chipseq boots without error with more than one contrast (upset module active)", {
  eselist <- make_chipseq_eselist(contrasts = TRUE, contrast_stats = TRUE)
  eselist@contrasts <- list(
    list(Variable = "condition", Group.1 = "control", Group.2 = "treated"),
    list(Variable = "condition", Group.1 = "treated", Group.2 = "control")
  )

  expect_no_error(
    withCallingHandlers(
      shiny::testServer(chipseq, args = list(id = "chipseq", eselist = eselist), {
        session$userData$plotFormat <- function() "png"
        session$elapse(400)
      }),
      warning = function(w) invokeRestart("muffleWarning")
    )
  )
})
