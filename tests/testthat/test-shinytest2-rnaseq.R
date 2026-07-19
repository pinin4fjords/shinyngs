# The full rnaseq app

test_that("the rnaseq app boots and shows its home tab", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-boot")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  expect_match(app$get_text("h3.shinyngs-eyebrow"), "Jump to analysis")
})

# pca module (exercises selectmatrix internally)

test_that("the PCA tab renders a scatterplot and its selectmatrix controls", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-pca")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "pca")
  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("rnaseq-pca-pca-scatter" %in% outputs)
  expect_true("rnaseq-pca-components-datatable" %in% outputs)
  expect_true("rnaseq-pca-pca-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); assert the scatter actually plots one
  # point per sample rather than just checking the output registered.
  scatter <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-pca-pca-scatter"),
    simplifyVector = FALSE
  )
  traces <- scatter$x$data
  expect_gt(length(traces), 0)

  point_traces <- Filter(function(tr) length(tr$x) > 0, traces)
  expect_gt(length(point_traces), 0)
  n_points <- sum(vapply(point_traces, function(tr) length(tr$x), integer(1)))
  expect_equal(n_points, 12)
  expect_equal(sum(vapply(point_traces, function(tr) length(tr$y), integer(1))), 12)

  # The components datatable lists one row per sample.
  components <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-pca-components-datatable"),
    simplifyVector = FALSE
  )
  expect_equal(length(components$x$data[[1]]), 12)
})

# heatmap module

test_that("the Clustering Heatmap tab renders an interactive heatmap", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-heatmap")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "Clustering Heatmap")
  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("rnaseq-heatmap-clustering-interactive_heatmap" %in% outputs)
  expect_true("rnaseq-heatmap-clustering-heatmap-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); the clustering heatmap's main trace is
  # a 12x12 sample-by-sample matrix, so assert that shape rather than just
  # checking the output registered.
  widget <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-heatmap-clustering-interactive_heatmap"),
    simplifyVector = FALSE
  )
  heatmap_traces <- Filter(function(tr) identical(tr$type, "heatmap"), widget$x$data)
  expect_gt(length(heatmap_traces), 0)

  # The main sample-by-sample trace is picked by row count, same as
  # splitAnnotationLegend() (R/heatmap.R) does to tell it apart from any
  # annotation-strip trace of the same type.
  row_counts <- vapply(heatmap_traces, function(tr) length(tr$z), integer(1))
  expect_equal(sum(row_counts == 12), 1)
  main_trace <- heatmap_traces[[which.max(row_counts)]]
  expect_equal(length(main_trace$x), 12)
  expect_equal(length(main_trace$y), 12)
})

# URL bookmarking round-trip is covered separately in
# test-shinytest2-bookmark.R (its own file, so its 40s timeouts and separate
# on-disk-app process don't make this file the parallel-worker bottleneck).
