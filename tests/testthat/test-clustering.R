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

# plotly_cluster_profiles()

make_cluster_fixture <- function() {
  set.seed(1)
  list(
    `3` = matrix(rnorm(12), nrow = 3, dimnames = list(paste0("g", 1:3), paste0("s", 1:4))),
    `7` = matrix(rnorm(8), nrow = 2, dimnames = list(paste0("g", 4:5), paste0("s", 1:4)))
  )
}

test_that("plotly_cluster_profiles draws a main line plus a shaded error band per cluster under 'filled_line'", {
  mbc <- make_cluster_fixture()

  built <- plotly::plotly_build(plotly_cluster_profiles(mbc, cluster_display = "filled_line", average_type = "mean", limits = "sd"))

  expect_length(built$x$data, 6)
  trace_names <- vapply(built$x$data, function(t) as.character(t$name), character(1))
  expect_equal(trace_names, c("Cluster 3", "mean + SD", "mean - SD", "Cluster 7", "mean + SD", "mean - SD"))
})

test_that("plotly_cluster_profiles draws one line-with-error-bars trace per cluster under 'error_bars'", {
  mbc <- make_cluster_fixture()

  built <- plotly::plotly_build(plotly_cluster_profiles(mbc, cluster_display = "error_bars", average_type = "mean", limits = "sd"))

  expect_length(built$x$data, 2)

  summarised <- summarySE(melt_matrix(mbc[["3"]]), measurevar = "value", groupvars = "Var2")
  cluster3 <- built$x$data[[which(vapply(built$x$data, function(t) identical(as.character(t$name), "Cluster 3"), logical(1)))]]

  expect_equal(as.numeric(cluster3$y), summarised$mean)
  expect_equal(as.numeric(cluster3$error_y$array), summarised$sd)
})

test_that("plotly_cluster_profiles derives the median-specific column without requiring a pre-prefixed limits argument", {
  mbc <- make_cluster_fixture()

  # limits = "sd" resolves against the median-specific column even without the caller
  # pre-prefixing it - a caller shouldn't need to know summarySE()'s internal column-naming
  # convention. median.sd comes from a bootstrap (bootstrapMedian()), so both calls need the
  # same seed to compare.
  set.seed(42)
  built <- plotly::plotly_build(plotly_cluster_profiles(mbc, cluster_display = "error_bars", average_type = "median", limits = "sd"))

  set.seed(42)
  summarised <- summarySE(melt_matrix(mbc[["3"]]), measurevar = "value", groupvars = "Var2", add_medians = TRUE)
  cluster3 <- built$x$data[[which(vapply(built$x$data, function(t) identical(as.character(t$name), "Cluster 3"), logical(1)))]]

  expect_equal(as.numeric(cluster3$y), summarised$median)
  expect_equal(as.numeric(cluster3$error_y$array), summarised$median.sd)
})

test_that("plotly_cluster_profiles indexes colors by cluster number, not by list position", {
  mbc <- make_cluster_fixture()
  colors <- paste0("color", 1:10)

  built <- plotly::plotly_build(plotly_cluster_profiles(mbc, cluster_display = "error_bars", colors = colors))

  cluster3 <- built$x$data[[which(vapply(built$x$data, function(t) identical(as.character(t$name), "Cluster 3"), logical(1)))]]
  cluster7 <- built$x$data[[which(vapply(built$x$data, function(t) identical(as.character(t$name), "Cluster 7"), logical(1)))]]

  expect_equal(cluster3$line$color, "color3")
  expect_equal(cluster7$line$color, "color7")
})

test_that("plotly_cluster_profiles subsamples large clusters to max_sample_lines under 'sample_lines'", {
  set.seed(1)
  big_cluster <- matrix(rnorm(50 * 4), nrow = 50, dimnames = list(paste0("g", 1:50), paste0("s", 1:4)))

  built <- plotly::plotly_build(plotly_cluster_profiles(list(`1` = big_cluster), cluster_display = "sample_lines", max_sample_lines = 10))

  # one trace per cluster; plotly separates the per-feature lines within it with NA gaps, so
  # only the non-NA points (subsampled features x samples) and distinct feature labels count
  trace <- built$x$data[[1]]
  expect_equal(sum(!is.na(trace$x)), 40)
  expect_length(unique(trace$text[!is.na(trace$text)]), 10)
})

test_that("plotly_cluster_profiles uses a precomputed summarised_matrices_by_cluster instead of recomputing summarySE()", {
  mbc <- make_cluster_fixture()

  # deliberately-wrong summary stats: if the precomputed values aren't honoured, the plot
  # would show the real mean/sd instead of these
  fake_summary <- lapply(mbc, function(m) {
    s <- summarySE(melt_matrix(m), measurevar = "value", groupvars = "Var2")
    s$mean <- 999
    s
  })

  built <- plotly::plotly_build(plotly_cluster_profiles(
    mbc,
    cluster_display = "error_bars", summarised_matrices_by_cluster = fake_summary
  ))

  cluster3 <- built$x$data[[which(vapply(built$x$data, function(t) identical(as.character(t$name), "Cluster 3"), logical(1)))]]
  expect_true(all(as.numeric(cluster3$y) == 999))
})
