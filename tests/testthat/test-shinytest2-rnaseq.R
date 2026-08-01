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

  expect_equal(app$get_value(input = "rnaseq-pca-pca-threedee"), "TRUE")

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

  point_traces <- Filter(function(tr) identical(tr$mode, "markers") && length(tr$text) > 0, traces)
  expect_gt(length(point_traces), 0)
  sample_labels <- unlist(lapply(point_traces, function(tr) unlist(tr$text, use.names = FALSE)), use.names = FALSE)
  n_points <- sum(vapply(point_traces, function(tr) length(tr$x), integer(1)))
  expect_setequal(sample_labels, paste0("sample", seq_len(12)))
  expect_length(sample_labels, 12)
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

test_that("the Gene info tab defaults single-contrast differential effects to the table", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-gene-contrast-profile")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "geneinfo")
  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-gene-gene_label-label` = "Gene1", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  tab_labels <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('#rnaseq-gene-differentialEffects_ui .nav-link')).map(e => e.textContent.trim())"
  ))

  expect_equal(tab_labels, "Table")
  expect_true(app$get_js("!!document.querySelector('#rnaseq-gene-geneContrastsTable-datatable table')"))
  expect_false(app$get_js("!!document.querySelector('#rnaseq-gene-geneContrastProfile')"))
})

# URL bookmarking round-trip is covered separately in
# test-shinytest2-bookmark.R (its own file, so its 40s timeouts and separate
# on-disk-app process don't make this file the parallel-worker bottleneck).
