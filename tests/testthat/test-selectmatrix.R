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

# idToLabel()

test_that("idToLabel combines the label field with the id, separated by sep", {
  ese <- make_labelled_ese()

  expect_equal(idToLabel(c("g1", "g2"), ese), c("GeneA / g1", "GeneB / g2"))
})

test_that("idToLabel falls back to the bare id where the label is NA", {
  ese <- make_labelled_ese()

  expect_equal(idToLabel("g3", ese), "g3")
})

test_that("idToLabel returns ids unchanged when no labelfield is set", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )

  expect_equal(idToLabel(c("g1", "g2"), ese), c("g1", "g2"))
})

test_that("idToLabel accepts a custom separator", {
  ese <- make_labelled_ese()
  expect_equal(idToLabel("g1", ese, sep = "-"), "GeneA-g1")
})

# convertIds()

test_that("convertIds maps row names to a metadata column", {
  ese <- make_labelled_ese()

  expect_equal(convertIds(c("g1", "g2"), ese, "gene_name"), c("GeneA", "GeneB"))
})

test_that("convertIds splits and re-joins space-separated multi-value ids", {
  ese <- make_labelled_ese()

  expect_equal(convertIds("g1 g2", ese, "gene_name"), "GeneA GeneB")
})

test_that("convertIds can drop unmatched/NA results", {
  ese <- make_labelled_ese()

  expect_equal(convertIds(c("g1", "g3"), ese, "gene_name", remove_na = TRUE), "GeneA")
})

# singleValidMatrix()

test_that("singleValidMatrix is TRUE for a single experiment with a single assay", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  expect_true(singleValidMatrix(eselist))
})

test_that("singleValidMatrix is FALSE when the single experiment has multiple assays", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat, norm = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  expect_false(singleValidMatrix(eselist))
})

test_that("singleValidMatrix is FALSE for multiple experiments", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(a = ese, b = ese))

  expect_false(singleValidMatrix(eselist))
})
