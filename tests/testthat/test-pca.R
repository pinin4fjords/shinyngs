# runPCA()

test_that("runPCA scales variables before running prcomp", {
  mat <- matrix(
    c(
      1, 2, 3, 4,
      10, 200, 3000, 40000,
      5, 6, 5, 7,
      100, 90, 80, 70
    ),
    nrow = 4, byrow = TRUE
  )
  rownames(mat) <- paste0("gene", 1:4)
  colnames(mat) <- paste0("sample", 1:4)

  pca <- runPCA(mat)

  logged <- log2(mat + 1)
  logged <- logged[apply(logged, 1, function(x) length(unique(x))) > 1, ]

  expected_scaled <- prcomp(t(as.matrix(logged)), scale. = TRUE)
  expect_equal(pca$sdev, expected_scaled$sdev)
  expect_equal(pca$rotation, expected_scaled$rotation)
  expect_equal(pca$x, expected_scaled$x)

  # Confirm the scaling argument is genuinely reaching prcomp: an unscaled
  # run on the same input produces different results.
  unscaled <- prcomp(t(as.matrix(logged)), scale. = FALSE)
  expect_false(isTRUE(all.equal(pca$sdev, unscaled$sdev)))
})

test_that("runPCA raises an informative error with fewer than 2 samples", {
  mat <- matrix(1:4, nrow = 4, ncol = 1)
  rownames(mat) <- paste0("gene", 1:4)
  colnames(mat) <- "sample1"

  expect_error(runPCA(mat), "at least 2 samples")
})

test_that("runPCA raises an informative error when no features vary across samples", {
  mat <- matrix(5, nrow = 4, ncol = 3)
  rownames(mat) <- paste0("gene", 1:4)
  colnames(mat) <- paste0("sample", 1:3)

  expect_error(runPCA(mat), "at least one feature with variable values")
})

# compile_pca_data()

test_that("compile_pca_data returns coordinates and percent variance for all genes when ntop is NULL", {
  set.seed(1)
  mat <- matrix(rexp(200, rate = .1), nrow = 20)
  rownames(mat) <- paste0("gene", 1:20)
  colnames(mat) <- paste0("sample", 1:10)

  result <- compile_pca_data(mat)

  expect_true(is.data.frame(result$coords))
  expect_equal(nrow(result$coords), ncol(mat))
  expect_equal(ncol(result$coords), ncol(mat))
  expect_equal(sum(result$percentVar), 100)
})

test_that("compile_pca_data restricts the PCA to the ntop most variable genes", {
  set.seed(1)
  mat <- matrix(rexp(200, rate = .1), nrow = 20)
  rownames(mat) <- paste0("gene", 1:20)
  colnames(mat) <- paste0("sample", 1:10)

  result <- compile_pca_data(mat, ntop = 5)
  full <- compile_pca_data(mat)

  # Using fewer genes changes the PCA result relative to using them all
  expect_false(isTRUE(all.equal(result$coords, full$coords)))

  # The selected genes should be exactly the 5 most variable ones
  top5 <- select_variable_genes(matrix = mat, ntop = 5)
  expected <- compile_pca_data(mat[top5, , drop = FALSE])
  expect_equal(result$coords, expected$coords)
})

# calculatePCAFractionExplained()

test_that("calculatePCAFractionExplained returns percentages that sum to 100", {
  set.seed(1)
  mat <- matrix(rnorm(40), nrow = 4)
  pca <- prcomp(mat, scale. = TRUE)

  fraction <- calculatePCAFractionExplained(pca)

  expect_equal(sum(fraction), 100)
  expect_equal(length(fraction), length(pca$sdev))
})

# interactive_screeplot()

test_that("interactive_screeplot draws one point per component with a single trace", {
  built <- plotly::plotly_build(interactive_screeplot(c(45, 25, 15, 10, 5)))

  expect_length(built$x$data, 1)
  expect_equal(as.character(built$x$data[[1]]$x), paste0("PC", 1:5))
  expect_equal(as.numeric(built$x$data[[1]]$y), c(45, 25, 15, 10, 5))
})

test_that("interactive_screeplot respects n_components", {
  built <- plotly::plotly_build(interactive_screeplot(c(45, 25, 15, 10, 5), n_components = 3))

  expect_equal(as.character(built$x$data[[1]]$x), paste0("PC", 1:3))
  expect_equal(as.numeric(built$x$data[[1]]$y), c(45, 25, 15))
})

test_that("interactive_screeplot adds a cumulative trace when requested", {
  built <- plotly::plotly_build(interactive_screeplot(c(45, 25, 15, 10, 5), cumulative = TRUE))

  expect_length(built$x$data, 2)
  expect_equal(as.numeric(built$x$data[[2]]$y), cumsum(c(45, 25, 15, 10, 5)))
})
