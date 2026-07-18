# calculateDist() / calculateDendrogram()

test_that("calculateDist returns a dist object with one entry per sample pair", {
  set.seed(1)
  mat <- matrix(rpois(50 * 5, lambda = 40) + 1, nrow = 50)
  colnames(mat) <- paste0("s", 1:5)
  rownames(mat) <- paste0("g", 1:50)

  d <- calculateDist(mat, cor_method = "pearson")

  expect_s3_class(d, "dist")
  expect_length(d, choose(5, 2))
})

test_that("calculateDendrogram produces an hclust tree with one merge per sample gap", {
  set.seed(1)
  mat <- matrix(rpois(50 * 5, lambda = 40) + 1, nrow = 50)
  colnames(mat) <- paste0("s", 1:5)
  rownames(mat) <- paste0("g", 1:50)

  hcd <- calculateDendrogram(mat, cor_method = "pearson", cluster_method = "average")

  expect_s3_class(hcd, "dendrogram")
  expect_setequal(labels(hcd), colnames(mat))

  hc <- as.hclust(hcd)
  expect_equal(nrow(hc$merge), ncol(mat) - 1)
})

# plotly_clusteringDendrogram()

test_that("plotly_clusteringDendrogram puts the branch tree in trace 0 and hides toggled-off groups", {
  set.seed(4)
  grps <- c("A", "B", "C")
  samp <- unlist(lapply(grps, function(g) paste0(g, 1:2)))
  mat <- matrix(rpois(300 * length(samp), lambda = 50) + 1, nrow = 300)
  colnames(mat) <- samp
  rownames(mat) <- paste0("gene", 1:300)
  experiment <- data.frame(row.names = samp, group = rep(grps, each = 2))

  built <- plotly::plotly_build(plotly_clusteringDendrogram(mat, experiment, colorby = "group", hidden_groups = "B", source = "test"))
  traces <- built$x$data

  # Trace 0 is always the branch tree, with one trace per group after it
  expect_equal(traces[[1]]$type, "scatter")
  expect_null(traces[[1]]$name)
  expect_length(traces, 4)

  visibility <- setNames(
    lapply(traces[-1], function(t) if (is.null(t$visible)) "TRUE" else as.character(t$visible)),
    vapply(traces[-1], function(t) t$name, character(1))
  )
  expect_equal(visibility[["B"]], "legendonly")
  expect_equal(visibility[["A"]], "TRUE")
  expect_equal(visibility[["C"]], "TRUE")

  # The hidden group's samples are dropped from the leaf axis, others remain
  expect_false(any(c("B1", "B2") %in% built$x$layout$xaxis$ticktext))
  expect_true(all(c("A1", "A2", "C1", "C2") %in% built$x$layout$xaxis$ticktext))
})

test_that("plotly_clusteringDendrogram falls back to a placeholder when fewer than two groups remain visible", {
  set.seed(5)
  samp <- c("A1", "A2", "B1", "B2", "C1")
  experiment <- data.frame(row.names = samp, group = c("A", "A", "B", "B", "C"))
  mat <- matrix(rpois(300 * length(samp), lambda = 50) + 1, nrow = 300)
  colnames(mat) <- samp
  rownames(mat) <- paste0("gene", 1:300)

  built <- suppressWarnings(plotly::plotly_build(plotly_clusteringDendrogram(mat, experiment, colorby = "group", hidden_groups = c("A", "B"), source = "test2")))

  expect_match(built$x$layout$annotations[[1]]$text, "Select at least two groups to cluster")

  # The branch trace draws nothing when there's nothing to cluster
  expect_null(built$x$data[[1]][["x", exact = TRUE]])
})
