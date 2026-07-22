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

# genesetselectInput()

test_that("genesetselectInput caps maxItems at 1 when multiple selection is disallowed", {
  single_html <- as.character(genesetselectInput("myid", multiple = FALSE))
  expect_match(single_html, '"maxItems":1')
})

test_that("genesetselectInput allows several items when multiple selection is allowed", {
  multi_html <- as.character(genesetselectInput("myid", multiple = TRUE))
  expect_match(multi_html, '"maxItems":5')
})

# restrict_geneset_choices()

test_that("restrict_geneset_choices returns choices unchanged when available_ids is NULL", {
  choices <- c("SET A (KEGG)" = "1-1", "SET B (KEGG)" = "1-2")
  codes_by_id <- c(SET_A = "1-1", SET_B = "1-2")

  expect_equal(restrict_geneset_choices(choices, codes_by_id, NULL), choices)
})

test_that("restrict_geneset_choices keeps only choices whose gene set ID is available", {
  choices <- c("SET A (KEGG)" = "1-1", "SET B (KEGG)" = "1-2", "SET C (KEGG)" = "1-3")
  codes_by_id <- c(SET_A = "1-1", SET_B = "1-2", SET_C = "1-3")

  restricted <- restrict_geneset_choices(choices, codes_by_id, c("SET_A", "SET_C"))

  expect_equal(restricted, c("SET A (KEGG)" = "1-1", "SET C (KEGG)" = "1-3"))
})

test_that("restrict_geneset_choices returns no choices when nothing is available", {
  choices <- c("SET A (KEGG)" = "1-1")
  codes_by_id <- c(SET_A = "1-1")

  expect_length(restrict_geneset_choices(choices, codes_by_id, character(0)), 0)
})

test_that("restrict_geneset_choices ignores available IDs that don't correspond to any gene set", {
  choices <- c("SET A (KEGG)" = "1-1")
  codes_by_id <- c(SET_A = "1-1")

  restricted <- restrict_geneset_choices(choices, codes_by_id, c("SET_A", "NOT_A_REAL_SET"))

  expect_equal(restricted, choices)
})
