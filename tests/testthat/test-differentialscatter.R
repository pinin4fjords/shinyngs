# annotateDifferentialTable()

test_that("annotateDifferentialTable marks matched, highlighted and hidden rows and labels only the visible ones", {
  mat <- matrix(1:6, nrow = 3, ncol = 2, dimnames = list(c("g1", "g2", "g3"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(grp = c("a", "b"), row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2", "g3"), row.names = c("g1", "g2", "g3")),
    idfield = "gene_id"
  )

  ct <- data.frame(value = c(1, 2, 3), row.names = c("g1", "g2", "g3"))

  contrast_reactives <- list(
    filteredContrastsTables = function() list(list(data.frame(value = 1, row.names = "g1")))
  )
  geneselect_reactives <- list(selectRows = function() "g2")
  selectmatrix_reactives <- list(getExperiment = function() ese)

  result <- annotateDifferentialTable(ct, contrast_reactives, geneselect_reactives, selectmatrix_reactives)

  expect_equal(as.character(result[c("g1", "g2", "g3"), "colorby"]), c(
    "match contrast filters", "in highlighted gene set", "hidden"
  ))
  expect_equal(result["g1", "label"], "g1")
  expect_equal(result["g2", "label"], "g2")
  expect_true(is.na(result["g3", "label"]))
})
