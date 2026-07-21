# interactive_heatmap()

test_that("interactive_heatmap handles single-row matrices (e.g. one informative sample variable)", {
  # A single-row matrix is the shape anova_pca_metadata() produces for the
  # "PCA vs Experiment" heatmap when only one sample metadata variable is
  # informative (e.g. a 2-vs-2 design with one grouping column) - see #61.
  pm <- matrix(
    c(0.01, 0.02, 0.03, 0.04),
    nrow = 1,
    dimnames = list("Experiment", c("PC1", "PC2", "PC3", "PC4"))
  )

  # heatmaply warns about the duplicated row names introduced by the
  # single-row workaround inside interactive_heatmap() - expected here.
  expect_warning(
    p <- interactive_heatmap(
      plotmatrix = pm, displaymatrix = pm, sample_annotation = NULL,
      cluster_rows = FALSE, cluster_cols = FALSE, scale = "row",
      row_labels = rownames(pm), hide_colorbar = TRUE
    ),
    "unique"
  )

  expect_s3_class(p, "plotly")
})

test_that("interactive_heatmap's show_row_labels toggles row tick labels but leaves hover text alone", {
  pm <- matrix(rnorm(20), nrow = 10, dimnames = list(paste0("gene", 1:10), paste0("s", 1:2)))

  shown <- plotly::plotly_build(interactive_heatmap(
    plotmatrix = pm, displaymatrix = pm, sample_annotation = NULL,
    cluster_rows = FALSE, cluster_cols = FALSE, row_labels = rownames(pm)
  ))
  hidden <- plotly::plotly_build(interactive_heatmap(
    plotmatrix = pm, displaymatrix = pm, sample_annotation = NULL,
    cluster_rows = FALSE, cluster_cols = FALSE, row_labels = rownames(pm), show_row_labels = FALSE
  ))

  yaxis_showticklabels <- function(built) {
    yaxes <- built$x$layout[grepl("^yaxis", names(built$x$layout))]
    unlist(lapply(yaxes, `[[`, "showticklabels"))
  }

  expect_true(all(yaxis_showticklabels(shown)))
  expect_false(any(yaxis_showticklabels(hidden)))
  expect_true(any(grepl("gene1<br>", unlist(lapply(hidden$x$data, `[[`, "text")))))
})

test_that("interactive_heatmap defaults plot_height to a value scaled to row count when labels are shown", {
  few_rows <- matrix(rnorm(20), nrow = 10, dimnames = list(paste0("gene", 1:10), paste0("s", 1:2)))
  many_rows <- matrix(rnorm(2000), nrow = 1000, dimnames = list(paste0("gene", 1:1000), paste0("s", 1:2)))

  p_few <- interactive_heatmap(
    plotmatrix = few_rows, displaymatrix = few_rows, sample_annotation = NULL,
    cluster_rows = FALSE, cluster_cols = FALSE, row_labels = rownames(few_rows)
  )
  p_many <- interactive_heatmap(
    plotmatrix = many_rows, displaymatrix = many_rows, sample_annotation = NULL,
    cluster_rows = FALSE, cluster_cols = FALSE, row_labels = rownames(many_rows)
  )

  expect_gt(p_many$height, p_few$height)
})

test_that("interactive_heatmap caps the default plot_height when row labels are hidden, regardless of row count", {
  # A large gene set with labels hidden (e.g. a differentialabundance report
  # heatmap) should stay compact - it only needs to show the colour pattern,
  # not render legible per-row text - so the default shouldn't keep growing
  # with row count the way it does when labels are shown (see #271).
  many_rows <- matrix(rnorm(2000), nrow = 1000, dimnames = list(paste0("gene", 1:1000), paste0("s", 1:2)))
  huge_rows <- matrix(rnorm(10000), nrow = 5000, dimnames = list(paste0("gene", 1:5000), paste0("s", 1:2)))

  p_many <- interactive_heatmap(
    plotmatrix = many_rows, displaymatrix = many_rows, sample_annotation = NULL,
    cluster_rows = FALSE, cluster_cols = FALSE, row_labels = rownames(many_rows), show_row_labels = FALSE
  )
  p_huge <- interactive_heatmap(
    plotmatrix = huge_rows, displaymatrix = huge_rows, sample_annotation = NULL,
    cluster_rows = FALSE, cluster_cols = FALSE, row_labels = rownames(huge_rows), show_row_labels = FALSE
  )

  expect_equal(p_many$height, p_huge$height)
  expect_lt(p_huge$height, 1500)
})

# interactive_pca_metadata_heatmap()

test_that("interactive_pca_metadata_heatmap colors by -log10(p) but keeps the raw p value as a cell note", {
  set.seed(1)
  pcameta <- data.frame(
    row.names = paste0("sample", 1:6),
    treatment = rep(c("control", "treated"), each = 3),
    batch = rep(c("a", "b"), 3)
  )
  pca_coords <- matrix(rnorm(6 * 3), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:3)))
  fraction_explained <- c(50, 30, 20)

  pvals <- anova_pca_metadata(pca_coords, pcameta, fraction_explained)

  built <- plotly::plotly_build(interactive_pca_metadata_heatmap(pca_coords, pcameta, fraction_explained))
  trace <- built$x$data[[1]]

  expect_s3_class(built, "plotly")
  expect_equal(trace$type, "heatmap")
  expect_equal(unname(trace$z["treatment", ]), unname(log10(pvals["treatment", ])))
  expect_true(grepl(paste("Value:", trimws(formatC(pvals["treatment", 1], format = "g", digits = 5))), trace$text["treatment", 1], fixed = TRUE))
})

test_that("interactive_pca_metadata_heatmap drops rows with a single value across all shown components only when clustering rows", {
  # A row where the anova produced the same p value (or NA) for every
  # component can't be clustered, and isn't informative to show anyway -
  # rowsWithMultipleValues() should drop it, but only when cluster_rows = TRUE
  pvals <- matrix(
    c(0.01, 0.02, 0.03, 0.5, 0.5, 0.5),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("treatment", "constant"), paste0("PC", 1:3))
  )
  testthat::local_mocked_bindings(anova_pca_metadata = function(...) pvals)

  # Filtering to a single row hits the same duplicated-row-name workaround
  # (and accompanying heatmaply warning) covered above for interactive_heatmap()
  expect_warning(
    clustered <- plotly::plotly_build(interactive_pca_metadata_heatmap(matrix(nrow = 0, ncol = 0), data.frame(), numeric(0), cluster_rows = TRUE)),
    "unique"
  )
  unclustered <- plotly::plotly_build(interactive_pca_metadata_heatmap(matrix(nrow = 0, ncol = 0), data.frame(), numeric(0), cluster_rows = FALSE))

  expect_false("constant" %in% rownames(clustered$x$data[[1]]$z))
  expect_true("constant" %in% rownames(unclustered$x$data[[1]]$z))
})

# anova_pca_metadata()

test_that("anova_pca_metadata uses plain PC column names, not percent-suffixed ones", {
  set.seed(1)
  pcameta <- data.frame(
    row.names = paste0("sample", 1:6),
    treatment = rep(c("control", "treated"), each = 3)
  )
  pca_coords <- matrix(rnorm(6 * 3), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:3)))

  pvals <- anova_pca_metadata(pca_coords, pcameta, fraction_explained = c(50, 30, 20))

  expect_equal(colnames(pvals), c("PC1", "PC2", "PC3"))
})

test_that("anova_pca_metadata's n_components limits and clamps the number of components tested", {
  set.seed(1)
  pcameta <- data.frame(
    row.names = paste0("sample", 1:6),
    treatment = rep(c("control", "treated"), each = 3)
  )
  pca_coords <- matrix(rnorm(6 * 5), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:5)))
  fraction_explained <- c(40, 25, 15, 12, 8)

  limited <- anova_pca_metadata(pca_coords, pcameta, fraction_explained, n_components = 3)
  expect_equal(colnames(limited), c("PC1", "PC2", "PC3"))

  clamped <- anova_pca_metadata(pca_coords, pcameta, fraction_explained, n_components = 20)
  expect_equal(ncol(clamped), 5)
})

# interactive_pca_variance_heatmap()

test_that("interactive_pca_variance_heatmap syncs the scree plot's x-categories with the heatmap's columns", {
  set.seed(1)
  pcameta <- data.frame(
    row.names = paste0("sample", 1:6),
    treatment = rep(c("control", "treated"), each = 3),
    batch = rep(c("a", "b"), 3)
  )
  pca_coords <- matrix(rnorm(6 * 4), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:4)))
  fraction_explained <- c(45, 25, 20, 10)

  # cluster_rows = FALSE keeps the heatmap side of the figure to a single
  # heatmap trace (no row dendrogram), so the scree trace is unambiguously
  # the only "scatter"-type trace in the combined figure.
  built <- plotly::plotly_build(interactive_pca_variance_heatmap(pca_coords, pcameta, fraction_explained, cluster_rows = FALSE, n_components = 4))

  trace_types <- vapply(built$x$data, function(trace) trace$type, character(1))
  scree_trace <- built$x$data[[which(trace_types == "scatter")[1]]]
  heatmap_trace <- built$x$data[[which(trace_types == "heatmap")[1]]]

  expect_equal(as.character(scree_trace$x), colnames(heatmap_trace$z))
})

test_that("interactive_pca_variance_heatmap respects n_components in both the scree and heatmap portions", {
  set.seed(1)
  pcameta <- data.frame(
    row.names = paste0("sample", 1:6),
    treatment = rep(c("control", "treated"), each = 3),
    batch = rep(c("a", "b"), 3)
  )
  pca_coords <- matrix(rnorm(6 * 5), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:5)))
  fraction_explained <- c(40, 25, 15, 12, 8)

  built <- plotly::plotly_build(interactive_pca_variance_heatmap(pca_coords, pcameta, fraction_explained, cluster_rows = FALSE, n_components = 2))

  trace_types <- vapply(built$x$data, function(trace) trace$type, character(1))
  scree_trace <- built$x$data[[which(trace_types == "scatter")[1]]]
  heatmap_trace <- built$x$data[[which(trace_types == "heatmap")[1]]]

  expect_equal(as.character(scree_trace$x), c("PC1", "PC2"))
  expect_equal(colnames(heatmap_trace$z), c("PC1", "PC2"))
})
