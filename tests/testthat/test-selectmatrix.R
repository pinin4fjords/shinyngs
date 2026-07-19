make_labelled_ese <- function() {
  mat <- matrix(1:6, nrow = 3, dimnames = list(c("g1", "g2", "g3"), c("s1", "s2")))
  annotation <- data.frame(
    gene_id = c("g1", "g2", "g3"),
    gene_name = c("GeneA", "GeneB", NA),
    row.names = c("g1", "g2", "g3")
  )
  ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name"
  )
}

# id_to_label()

test_that("id_to_label combines the label field with the id, separated by sep", {
  ese <- make_labelled_ese()

  expect_equal(id_to_label(c("g1", "g2"), ese), c("GeneA / g1", "GeneB / g2"))
})

test_that("id_to_label falls back to the bare id where the label is NA", {
  ese <- make_labelled_ese()

  expect_equal(id_to_label("g3", ese), "g3")
})

test_that("id_to_label returns ids unchanged when no labelfield is set", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )

  expect_equal(id_to_label(c("g1", "g2"), ese), c("g1", "g2"))
})

test_that("id_to_label accepts a custom separator", {
  ese <- make_labelled_ese()
  expect_equal(id_to_label("g1", ese, sep = "-"), "GeneA-g1")
})

# convert_ids()

test_that("convert_ids maps row names to a metadata column", {
  ese <- make_labelled_ese()

  expect_equal(convert_ids(c("g1", "g2"), ese, "gene_name"), c("GeneA", "GeneB"))
})

test_that("convert_ids splits and re-joins space-separated multi-value ids", {
  ese <- make_labelled_ese()

  expect_equal(convert_ids("g1 g2", ese, "gene_name"), "GeneA GeneB")
})

test_that("convert_ids can drop unmatched/NA results", {
  ese <- make_labelled_ese()

  expect_equal(convert_ids(c("g1", "g3"), ese, "gene_name", remove_na = TRUE), "GeneA")
})

# single_valid_matrix()

test_that("single_valid_matrix is TRUE for a single experiment with a single assay", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  expect_true(single_valid_matrix(eselist))
})

test_that("single_valid_matrix is FALSE when the single experiment has multiple assays", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat, norm = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  expect_false(single_valid_matrix(eselist))
})

test_that("single_valid_matrix is FALSE for multiple experiments", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(a = ese, b = ese))

  expect_false(single_valid_matrix(eselist))
})
