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
