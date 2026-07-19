# the standalone "heatmap" app type takes prepare_app()'s simpleApp() branch
# instead of the multi-panel one (a single nav_panel wrapping one module's
# Input/Output directly, under the "pages" nav id rather than "<type>-<type>"),
# so it needs its own smoke test rather than reusing the rnaseq-embedded
# heatmap tab test in test-shinytest2-rnaseq.R

test_that("the standalone heatmap app renders an interactive heatmap", {
  skip_on_cran()

  app <- shinytest2_app_driver("heatmap", "heatmap-standalone")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("heatmap-interactive_heatmap" %in% outputs)
  expect_true("heatmap-heatmap-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); the expression heatmap's main trace is
  # a gene-by-sample matrix, so its column count should match sample number.
  widget <- jsonlite::fromJSON(
    app$get_value(output = "heatmap-interactive_heatmap"),
    simplifyVector = FALSE
  )
  heatmap_traces <- Filter(function(tr) identical(tr$type, "heatmap"), widget$x$data)
  expect_gt(length(heatmap_traces), 0)

  main_trace <- heatmap_traces[[which.max(vapply(heatmap_traces, function(tr) length(tr$z), integer(1)))]]
  expect_equal(length(main_trace$x), 12)
})
