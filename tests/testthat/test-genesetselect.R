# prettify_gene_set_name()

test_that("prettify_gene_set_name keeps the first word and lower-cases the rest", {
  expect_equal(prettify_gene_set_name("KEGG_GLYCOLYSIS_GLUCONEOGENESIS"), "KEGG glycolysis gluconeogenesis")
})

test_that("prettify_gene_set_name leaves a single-word name with a trailing space", {
  expect_equal(prettify_gene_set_name("KEGG"), "KEGG ")
})

test_that("prettify_gene_set_name vectorises over multiple gene set names", {
  expect_equal(
    prettify_gene_set_name(c("KEGG_GLYCOLYSIS", "GO_CELL_CYCLE")),
    c("KEGG glycolysis", "GO cell cycle")
  )
})
