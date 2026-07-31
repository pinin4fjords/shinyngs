# ExploratorySummarizedExperimentList()

test_that("ExploratorySummarizedExperimentList leaves default_groupvar empty when no grouping column is found", {
  n_samples <- 4
  counts <- matrix(seq_len(6 * n_samples), nrow = 6)
  rownames(counts) <- paste0("gene", 1:6)
  colnames(counts) <- paste0("s", seq_len(n_samples))

  # A column with no repeated values gives choose_grouping_variables() nothing
  # to pick, so group_vars is auto-detected as empty.
  coldata <- S4Vectors::DataFrame(row.names = colnames(counts), uid = paste0("u", seq_len(n_samples)))
  annotation <- data.frame(gene_id = rownames(counts), row.names = rownames(counts))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(eses = list(counts = ese))

  expect_length(eselist@group_vars, 0)
  expect_length(eselist@default_groupvar, 0)
  expect_false(has_slot_data(eselist, "default_groupvar"))
})

test_that("ExploratorySummarizedExperimentList subsetting preserves metadata slots", {
  inputs <- make_test_ese_inputs()
  ese <- do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id")))
  eselist <- ExploratorySummarizedExperimentList(
    eses = list(first = ese, second = ese),
    static_pdf = "study.pdf",
    group_vars = "group",
    default_groupvar = "group"
  )

  expect_equal(eselist["first"]@static_pdf, "study.pdf")
  expect_equal(eselist[1]@static_pdf, "study.pdf")
  expect_equal(eselist[c(TRUE, FALSE)]@static_pdf, "study.pdf")
})

test_that("ExploratorySummarizedExperimentList validates grouping fields", {
  inputs <- make_test_ese_inputs()
  ese <- do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id")))

  expect_error(
    ExploratorySummarizedExperimentList(eses = list(ese), group_vars = "missing", default_groupvar = "missing"),
    "group_vars fields are absent"
  )
})
