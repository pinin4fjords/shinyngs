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
