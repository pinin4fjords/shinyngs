# melt_matrix()

test_that("melt_matrix reshapes a matrix to long format with named dimensions", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

  out <- melt_matrix(m)

  expect_equal(colnames(out), c("Var1", "Var2", "value"))
  expect_equal(nrow(out), 6)
  expect_equal(out$value, as.integer(m))
  expect_equal(as.character(out$Var1), rep(c("r1", "r2"), 3))
})

test_that("melt_matrix uses integer indices when dimnames are absent", {
  m <- matrix(1:4, nrow = 2)

  out <- melt_matrix(m)

  expect_true(is.integer(out$Var1))
  expect_true(is.integer(out$Var2))
  expect_equal(out$Var1, rep(1:2, 2))
})

test_that("melt_matrix accepts custom varnames and value.name", {
  m <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))

  out <- melt_matrix(m, varnames = c("gene", "sample"), value.name = "expr")

  expect_equal(colnames(out), c("gene", "sample", "expr"))
})

# interleaveColumns()

test_that("interleaveColumns alternates columns from the two input matrices", {
  mat1 <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("a1", "a2")))
  mat2 <- matrix(5:8, nrow = 2, dimnames = list(NULL, c("b1", "b2")))

  result <- interleaveColumns(mat1, mat2)

  expect_equal(colnames(result), c("a1", "b1", "a2", "b2"))
  expect_equal(result[, "a1"], mat1[, "a1"])
  expect_equal(result[, "b2"], mat2[, "b2"])
})

test_that("interleaveColumns preserves row count and dimension", {
  mat1 <- matrix(1:6, nrow = 3, dimnames = list(NULL, c("a1", "a2")))
  mat2 <- matrix(7:12, nrow = 3, dimnames = list(NULL, c("b1", "b2")))

  result <- interleaveColumns(mat1, mat2)

  expect_equal(dim(result), c(3, 4))
})

# ggplotify()

test_that("ggplotify melts a matrix, dropping non-positive values", {
  mat <- matrix(c(0, 2, 4, 8), nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  experiment <- data.frame(row.names = c("s1", "s2"), grp = c("A", "B"))

  out <- ggplotify(mat, experiment, should_transform = FALSE)

  expect_true(all(c("gene", "name", "value", "type") %in% colnames(out)))
  expect_equal(nrow(out), 3) # the single zero value (g1/s1) is dropped
  expect_true(all(out$value > 0))
})

test_that("ggplotify adds a colorby column, ordering samples by group", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  experiment <- data.frame(row.names = c("s1", "s2"), grp = c("B", "A"))

  out <- ggplotify(mat, experiment, colorby = "grp", should_transform = FALSE)

  expect_true("colorby" %in% colnames(out))
  expect_equal(levels(out$colorby), c("B", "A")) # experiment's own row order, not alphabetical
})

test_that("ggplotify annotates sample names with their group when requested", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  experiment <- data.frame(row.names = c("s1", "s2"), grp = c("A", "B"))

  out <- ggplotify(mat, experiment, colorby = "grp", annotate_samples = TRUE, should_transform = FALSE)

  expect_true(all(grepl("\\(A\\)|\\(B\\)", as.character(out$name))))
})

test_that("ggplotify respects should_transform = TRUE by log2-transforming values", {
  mat <- matrix(c(1, 2, 4, 8), nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  experiment <- data.frame(row.names = c("s1", "s2"))

  out <- ggplotify(mat, experiment, should_transform = TRUE)

  expect_equal(sort(out$value), sort(log2(c(1, 2, 4, 8))))
})

test_that("ggplotify computes densities when value_type is 'density'", {
  set.seed(1)
  mat <- matrix(rpois(40, lambda = 20) + 1, nrow = 20, dimnames = list(paste0("g", 1:20), c("s1", "s2")))
  experiment <- data.frame(row.names = c("s1", "s2"))

  out <- ggplotify(mat, experiment, value_type = "density", should_transform = FALSE)

  expect_true(all(c("name", "value", "density") %in% colnames(out)))
})

test_that("ggplotify computes a density trace when a sample has zero values", {
  # rmzeros turns zeros into NA ahead of the log2 transform, so a sample
  # with any zero entries reproduces the crash from issue #249 unless
  # those NAs are dropped before stats::density() is called.
  mat <- matrix(
    c(0, 10, 100, 1000, 5, 50, 500, 5000),
    nrow = 4, dimnames = list(paste0("gene", 1:4), c("s1", "s2"))
  )
  experiment <- data.frame(row.names = c("s1", "s2"))

  plotdata <- ggplotify(mat, experiment, value_type = "density")

  expect_setequal(unique(plotdata$name), c("s1", "s2"))
  expect_true(all(is.finite(plotdata$density)))
})

test_that("ggplotify concatenates a named list of matrices, tagging each with its type", {
  mat1 <- matrix(c(1, 2), nrow = 1, dimnames = list("g1", c("s1", "s2")))
  mat2 <- matrix(c(3, 4), nrow = 1, dimnames = list("g1", c("s1", "s2")))
  experiment <- data.frame(row.names = c("s1", "s2"))

  out <- ggplotify(list(Raw = mat1, Filtered = mat2), experiment, should_transform = FALSE)

  expect_setequal(as.character(unique(out$type)), c("Raw", "Filtered"))
  expect_equal(levels(out$type), c("Raw", "Filtered")) # first-seen order preserved
})
