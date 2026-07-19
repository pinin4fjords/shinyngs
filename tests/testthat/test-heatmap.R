# interactiveHeatmap()

test_that("interactiveHeatmap handles single-row matrices (e.g. one informative sample variable)", {
  # A single-row matrix is the shape anova_pca_metadata() produces for the
  # "PCA vs Experiment" heatmap when only one sample metadata variable is
  # informative (e.g. a 2-vs-2 design with one grouping column) - see #61.
  pm <- matrix(
    c(0.01, 0.02, 0.03, 0.04),
    nrow = 1,
    dimnames = list("Experiment", c("PC1 (39%)", "PC2 (32.2%)", "PC3 (28.8%)", "PC4 (0%)"))
  )

  # heatmaply warns about the duplicated row names introduced by the
  # single-row workaround inside interactiveHeatmap() - expected here.
  expect_warning(
    p <- interactiveHeatmap(
      plotmatrix = pm, displaymatrix = pm, sample_annotation = NULL,
      cluster_rows = FALSE, cluster_cols = FALSE, scale = "row",
      row_labels = rownames(pm), hide_colorbar = TRUE
    ),
    "unique"
  )

  expect_s3_class(p, "plotly")
})

# plotly_pca_metadata_heatmap()

test_that("plotly_pca_metadata_heatmap colors by -log10(p) but keeps the raw p value as a cell note", {
  set.seed(1)
  pcameta <- data.frame(
    row.names = paste0("sample", 1:6),
    treatment = rep(c("control", "treated"), each = 3),
    batch = rep(c("a", "b"), 3)
  )
  pca_coords <- matrix(rnorm(6 * 3), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:3)))
  fraction_explained <- c(50, 30, 20)

  pvals <- anova_pca_metadata(pca_coords, pcameta, fraction_explained)

  built <- plotly::plotly_build(plotly_pca_metadata_heatmap(pca_coords, pcameta, fraction_explained))
  trace <- built$x$data[[1]]

  expect_s3_class(built, "plotly")
  expect_equal(trace$type, "heatmap")
  expect_equal(unname(trace$z["treatment", ]), unname(log10(pvals["treatment", ])))
  expect_true(grepl(paste("Value:", trimws(formatC(pvals["treatment", 1], format = "g", digits = 5))), trace$text["treatment", 1], fixed = TRUE))
})

test_that("plotly_pca_metadata_heatmap drops rows with a single value across all shown components only when clustering rows", {
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
  # (and accompanying heatmaply warning) covered above for interactiveHeatmap()
  expect_warning(
    clustered <- plotly::plotly_build(plotly_pca_metadata_heatmap(matrix(nrow = 0, ncol = 0), data.frame(), numeric(0), cluster_rows = TRUE)),
    "unique"
  )
  unclustered <- plotly::plotly_build(plotly_pca_metadata_heatmap(matrix(nrow = 0, ncol = 0), data.frame(), numeric(0), cluster_rows = FALSE))

  expect_false("constant" %in% rownames(clustered$x$data[[1]]$z))
  expect_true("constant" %in% rownames(unclustered$x$data[[1]]$z))
})
