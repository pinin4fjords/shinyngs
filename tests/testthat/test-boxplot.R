# box_summary()

test_that("box_summary computes quartiles, whiskers and outliers", {
  s <- box_summary(c(1:10, 100), labels = paste0("g", 1:11))

  expect_equal(s$q1, 3.5)
  expect_equal(s$median, 6)
  expect_equal(s$q3, 8.5)

  # Whiskers extend to the most extreme value still within 1.5 * IQR, not to
  # the fence boundary itself
  expect_equal(s$lowerfence, 1)
  expect_equal(s$upperfence, 10)

  expect_equal(s$outlier_values, 100)
  expect_equal(s$outlier_labels, "g11")
})

test_that("box_summary drops non-finite values", {
  with_na <- box_summary(c(1:10, 100, NA), labels = paste0("g", 1:12))
  clean <- box_summary(c(1:10, 100), labels = paste0("g", 1:11))
  expect_equal(with_na$q1, clean$q1)
  expect_equal(with_na$upperfence, clean$upperfence)
})

test_that("box_summary returns NA stats for an all-missing vector", {
  s <- box_summary(c(NA_real_, NA_real_), labels = c("a", "b"))
  expect_true(is.na(s$median))
  expect_length(s$outlier_values, 0)
})

test_that("box_summary respects whisker_distance", {
  tight <- box_summary(c(1:10, 20), labels = paste0("g", 1:11), whisker_distance = 0.5)
  loose <- box_summary(c(1:10, 20), labels = paste0("g", 1:11), whisker_distance = 3)
  expect_gt(length(tight$outlier_values), length(loose$outlier_values))
})

# plotly_boxplot()

test_that("plotly_boxplot draws precomputed boxes without shipping raw data", {
  set.seed(1)
  mat <- matrix(rpois(2000 * 6, lambda = 50) + 1, nrow = 2000)
  colnames(mat) <- paste0("s", 1:6)
  rownames(mat) <- paste0("gene", 1:2000)
  experiment <- data.frame(row.names = colnames(mat), group = rep(c("A", "B"), each = 3))

  built <- plotly::plotly_build(plotly_boxplot(mat, experiment, colorby = "group"))
  box_traces <- Filter(function(t) identical(t$type, "box"), built$x$data)

  # One box trace per group
  expect_length(box_traces, 2)

  # Boxes are drawn from precomputed statistics
  expect_false(is.null(box_traces[[1]]$q1))
  expect_false(is.null(box_traces[[1]]$median))
  expect_false(is.null(box_traces[[1]]$q3))

  # None of the box traces carry the underlying per-gene values, so the payload
  # is independent of the number of rows in the matrix
  for (tr in box_traces) {
    expect_lt(length(tr$q1) + length(tr$median), 10)
  }
})

test_that("plotly_boxplot handles no grouping and multiple matrices", {
  set.seed(2)
  mat <- matrix(rpois(500 * 4, lambda = 50) + 1, nrow = 500)
  colnames(mat) <- paste0("s", 1:4)
  rownames(mat) <- paste0("gene", 1:500)
  experiment <- data.frame(row.names = colnames(mat), group = rep(c("A", "B"), each = 2))

  expect_s3_class(plotly_boxplot(mat, experiment), "plotly")

  # The multi-matrix path assembles a plotly subplot, which emits a benign
  # internal "No source found" warning
  expect_s3_class(
    suppressWarnings(plotly_boxplot(list(Raw = mat, Filtered = mat[1:250, ]), experiment, colorby = "group")),
    "plotly"
  )
})

test_that("boxplotGroupLevels returns grouping levels in first-seen order", {
  experiment <- data.frame(row.names = paste0("s", 1:4), group = c("B", "A", "B", "A"))
  expect_equal(boxplotGroupLevels(experiment, "group"), c("B", "A"))
  expect_equal(boxplotGroupLevels(experiment, NULL), " ")
})

test_that("plotly_boxplot drops hidden groups but keeps them in the legend", {
  set.seed(4)
  grps <- c("A", "B", "C")
  samp <- unlist(lapply(grps, function(g) paste0(g, 1:2)))
  mat <- matrix(rpois(300 * length(samp), lambda = 50) + 1, nrow = 300)
  colnames(mat) <- samp
  rownames(mat) <- paste0("gene", 1:300)
  experiment <- data.frame(row.names = samp, group = rep(grps, each = 2))

  built <- plotly::plotly_build(plotly_boxplot(mat, experiment, colorby = "group", hidden_groups = "B"))
  box_traces <- Filter(function(t) identical(t$type, "box"), built$x$data)
  visibility <- setNames(lapply(box_traces, function(t) as.character(t$visible)), vapply(box_traces, function(t) t$name, character(1)))

  # The hidden group stays present (so it keeps a legend entry) but as legendonly
  expect_length(box_traces, 3)
  expect_equal(visibility[["B"]], "legendonly")
  expect_equal(visibility[["A"]], "TRUE")

  # Its samples are dropped from the drawn x axis
  expect_false(any(c("B1", "B2") %in% built$x$layout$xaxis$categoryarray))
  expect_true(all(c("A1", "A2", "C1", "C2") %in% built$x$layout$xaxis$categoryarray))
})

test_that("plotly_boxplot bounds the number of outliers drawn", {
  set.seed(3)
  # A heavy tail guarantees many points beyond the whiskers
  mat <- matrix(c(rpois(1000 * 3, lambda = 5), rpois(1000 * 3, lambda = 500)), nrow = 2000)
  colnames(mat) <- paste0("s", 1:3)
  rownames(mat) <- paste0("gene", 1:2000)
  experiment <- data.frame(row.names = colnames(mat), group = rep("A", 3))

  built <- plotly::plotly_build(plotly_boxplot(mat, experiment, colorby = "group", max_outliers = 20))
  outlier_traces <- Filter(function(t) identical(t$type, "scatter"), built$x$data)
  total_outliers <- sum(vapply(outlier_traces, function(t) length(t$y), integer(1)))
  expect_lte(total_outliers, 20)
})
