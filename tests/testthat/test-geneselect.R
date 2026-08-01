# select_variable_genes()

test_that("select_variable_genes selects the indices of the most variable rows", {
  mat <- matrix(
    c(
      1, 1, 1, 1, # no variance
      1, 2, 3, 4, # some variance
      1, 100, 1, 100, # highest variance
      1, 1, 2, 1 # low variance
    ),
    nrow = 4, byrow = TRUE
  )

  expect_equal(select_variable_genes(ntop = 1, matrix = mat), 3)
  expect_equal(select_variable_genes(ntop = 2, matrix = mat), c(3, 2))
})

test_that("select_variable_genes caps ntop at the number of available rows", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)

  result <- select_variable_genes(ntop = 10, matrix = mat)

  expect_equal(sort(result), c(1, 2))
})

test_that("select_variable_genes accepts precalculated row variances", {
  row_variances <- c(gene1 = 0.1, gene2 = 5, gene3 = 1)

  result <- select_variable_genes(ntop = 2, row_variances = row_variances)

  expect_equal(result, c(2, 3))
})

test_that("select_variable_genes errors when neither matrix nor row_variances is supplied", {
  expect_error(
    select_variable_genes(ntop = 2),
    "a value must be provided for either matrix or row_variances"
  )
})

test_that("variance_slider_range keeps the default within a small assay", {
  expect_equal(
    variance_slider_range(value = 50, maximum = 8),
    list(min = 8, max = 8, value = 8)
  )
})

test_that("variance_slider_range preserves the standard range for larger assays", {
  expect_equal(
    variance_slider_range(value = 50, maximum = 100),
    list(min = 10, max = 100, value = 50)
  )
})

test_that("variance_slider_range remains valid for an empty assay", {
  expect_equal(
    variance_slider_range(value = 50, maximum = 0),
    list(min = 1, max = 1, value = 1)
  )
})

test_that("geneselect uses the displayed default while its input initialises", {
  eselist <- make_medium_module_eselist()
  ese <- eselist[[1]]

  shiny::testServer(
    geneselect,
    args = list(
      id = "genes",
      eselist = eselist,
      getExperiment = shiny::reactive(ese),
      selectSamples = shiny::reactive(colnames(ese)),
      getAssay = shiny::reactive("counts")
    ),
    {
      expect_equal(getGeneSelect(), "all")
    }
  )
})
