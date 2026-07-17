# runClustering()

test_that("runClustering partitions rows into the requested number of clusters", {
  set.seed(1)
  mat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  rownames(mat) <- paste0("gene", 1:10)
  colnames(mat) <- paste0("sample", 1:6)

  clusters <- runClustering(mat, 3)

  expect_equal(length(unique(clusters$clustering)), 3)
  expect_equal(length(clusters$clustering), nrow(mat))
})

test_that("runClustering raises an informative error when more clusters are requested than features", {
  mat <- matrix(rnorm(24), nrow = 4, ncol = 6)
  rownames(mat) <- paste0("gene", 1:4)
  colnames(mat) <- paste0("sample", 1:6)

  expect_error(runClustering(mat, 6), "must be between 1 and the number of available features")
})

test_that("runClustering raises an informative error with fewer than 2 features", {
  mat <- matrix(rnorm(6), nrow = 1, ncol = 6)
  rownames(mat) <- "gene1"
  colnames(mat) <- paste0("sample", 1:6)

  expect_error(runClustering(mat, 1), "at least 2 features")
})
