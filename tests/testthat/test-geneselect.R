# selectVariableGenes()

test_that("selectVariableGenes selects the indices of the most variable rows", {
  mat <- matrix(
    c(
      1, 1, 1, 1, # no variance
      1, 2, 3, 4, # some variance
      1, 100, 1, 100, # highest variance
      1, 1, 2, 1 # low variance
    ),
    nrow = 4, byrow = TRUE
  )

  expect_equal(selectVariableGenes(ntop = 1, matrix = mat), 3)
  expect_equal(selectVariableGenes(ntop = 2, matrix = mat), c(3, 2))
})

test_that("selectVariableGenes caps ntop at the number of available rows", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)

  result <- selectVariableGenes(ntop = 10, matrix = mat)

  expect_equal(sort(result), c(1, 2))
})

test_that("selectVariableGenes accepts precalculated row variances", {
  row_variances <- c(gene1 = 0.1, gene2 = 5, gene3 = 1)

  result <- selectVariableGenes(ntop = 2, row_variances = row_variances)

  expect_equal(result, c(2, 3))
})

test_that("selectVariableGenes errors when neither matrix nor row_variances is supplied", {
  expect_error(
    selectVariableGenes(ntop = 2),
    "a value must be provided for either matrix or row_variances"
  )
})
