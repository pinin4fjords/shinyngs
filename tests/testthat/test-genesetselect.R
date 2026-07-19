# prettifyGeneSetName()

test_that("prettifyGeneSetName keeps the first word and lower-cases the rest", {
  expect_equal(prettifyGeneSetName("KEGG_GLYCOLYSIS_GLUCONEOGENESIS"), "KEGG glycolysis gluconeogenesis")
})

test_that("prettifyGeneSetName leaves a single-word name with a trailing space", {
  expect_equal(prettifyGeneSetName("KEGG"), "KEGG ")
})

test_that("prettifyGeneSetName vectorises over multiple gene set names", {
  expect_equal(
    prettifyGeneSetName(c("KEGG_GLYCOLYSIS", "GO_CELL_CYCLE")),
    c("KEGG glycolysis", "GO cell cycle")
  )
})
