# The full rnaseq app

test_that("the rnaseq app boots and shows its home tab", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-boot")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  expect_match(app$get_text("h3.shinyngs-eyebrow"), "Jump to analysis")
})

# the other "big" app types (chipseq, illuminaarray) share prepareApp()'s
# multi-panel branch with rnaseq, so a boot + one composed-module check each
# is enough to confirm the branch wires up correctly for their platform label
# and homeTab()/PCA composition, without re-testing every rnaseq-covered panel

test_that("the chipseq app boots and shows its ChIP-seq home tab", {
  skip_on_cran()

  app <- shinytest2_app_driver("chipseq", "chipseq-boot")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  expect_match(app$get_text("h3.shinyngs-eyebrow"), "Jump to analysis")
  expect_match(app$get_text("body"), "downstream ChIP-seq")
})

test_that("the chipseq app's PCA tab renders a scatterplot", {
  skip_on_cran()

  app <- shinytest2_app_driver("chipseq", "chipseq-pca")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`chipseq-chipseq` = "pca")
  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("chipseq-pca-pca-scatter" %in% outputs)
})

test_that("the illuminaarray app boots and shows its expression array home tab", {
  skip_on_cran()

  app <- shinytest2_app_driver("illuminaarray", "illuminaarray-boot")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  expect_match(app$get_text("h3.shinyngs-eyebrow"), "Jump to analysis")
  expect_match(app$get_text("body"), "downstream expression array")
})

test_that("the illuminaarray app's PCA tab renders a scatterplot", {
  skip_on_cran()

  app <- shinytest2_app_driver("illuminaarray", "illuminaarray-pca")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`illuminaarray-illuminaarray` = "pca")
  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("illuminaarray-pca-pca-scatter" %in% outputs)
})

# the standalone "heatmap" app type takes prepareApp()'s simpleApp() branch
# instead (a single nav_panel wrapping one module's Input/Output directly,
# under the "pages" nav id rather than "<type>-<type>"), so it needs its own
# smoke test rather than reusing the rnaseq-embedded heatmap tab test below

test_that("the standalone heatmap app renders an interactive heatmap", {
  skip_on_cran()

  app <- shinytest2_app_driver("heatmap", "heatmap-standalone")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("heatmap-interactiveHeatmap" %in% outputs)
  expect_true("heatmap-heatmap-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); the expression heatmap's main trace is
  # a gene-by-sample matrix, so its column count should match sample number.
  widget <- jsonlite::fromJSON(
    app$get_value(output = "heatmap-interactiveHeatmap"),
    simplifyVector = FALSE
  )
  heatmap_traces <- Filter(function(tr) identical(tr$type, "heatmap"), widget$x$data)
  expect_gt(length(heatmap_traces), 0)

  main_trace <- heatmap_traces[[which.max(vapply(heatmap_traces, function(tr) length(tr$z), integer(1)))]]
  expect_equal(length(main_trace$x), 12)
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
  expect_true("rnaseq-heatmap-clustering-interactiveHeatmap" %in% outputs)
  expect_true("rnaseq-heatmap-clustering-heatmap-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); the clustering heatmap's main trace is
  # a 12x12 sample-by-sample matrix, so assert that shape rather than just
  # checking the output registered.
  widget <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-heatmap-clustering-interactiveHeatmap"),
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

# selectmatrix + contrasts modules, as composed by differentialtable()
#
# Driven via shiny::testServer() rather than AppDriver: the differential
# table tab's contrast filter is inserted client-side via insertUI(), and
# reliably reproducing that DOM insertion under headless chromote (including
# its selectize.js-backed dropdown) needs more investigation than this PR's
# scope covers. testServer exercises the same server-side reactive graph
# directly and deterministically.

# URL bookmarking round-trip
#
# Runs from an on-disk app.R (see shinytest2_bookmark_app_driver) so bookmarking
# is enabled at session construction, as in real use. Restored values are read
# from the DOM: updateNumericInput() etc. set the control's value on restore but
# the client input registry only reflects it once the input reports.

test_that("a bookmark URL restores the active tab and a contrast filter value", {
  skip_on_cran()

  app <- shinytest2_bookmark_app_driver("rnaseq", "rnaseq-bookmark")
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 40000)

  expect_true(app$get_js("!!document.getElementById('shinyngs_share_view')"))

  app$set_inputs(`rnaseq-rnaseq` = "diff_tables")
  app$wait_for_idle(timeout = 40000)
  app$set_inputs(`rnaseq-differential-differential-fold_change0` = 7)
  app$wait_for_idle(timeout = 40000)

  app$click("shinyngs_share_view")
  app$wait_for_idle(timeout = 40000)
  url <- app$get_js("window.location.href")

  expect_match(url, "\\?", info = "bookmark should put state in the address bar")
  expect_true(grepl("contrast_filtersets", utils::URLdecode(url)))
  expect_false(grepl("plotly_", url), info = "transient plotly inputs are excluded")
  expect_false(grepl("_rows_", url), info = "transient DataTable inputs are excluded")

  # A full page navigation tears down the old page's Shiny JS object and boots
  # a fresh session; wait_for_idle() errors ("An error occurred while waiting
  # for Shiny to be stable") if called immediately afterwards, since there is
  # no stable Shiny busy-state to observe yet. Poll the DOM directly instead.
  app$run_js(sprintf("window.location.href = %s;", jsonlite::toJSON(url, auto_unbox = TRUE)))

  restored_condition <- paste(
    "(function(){",
    "var e = document.getElementById('rnaseq-differential-differential-fold_change0');",
    "return !!e && e.value === '7';",
    "})()"
  )
  app$wait_for_js(restored_condition, timeout = 40000, interval = 200)

  restored <- app$get_js(
    "(function(){var e=document.getElementById('rnaseq-differential-differential-fold_change0'); return e ? String(e.value) : 'NOEL';})()"
  )
  expect_equal(restored, "7")
  expect_equal(
    app$get_js("(window.Shiny && Shiny.shinyapp) ? String(Shiny.shinyapp.$inputValues['rnaseq-rnaseq']) : 'NA'"),
    "diff_tables"
  )
})

test_that("differentialtable computes a table without error given a real contrast", {
  skip_on_cran()

  eselist <- shinytest2_eselist()

  shiny::testServer(differentialtable, args = list(id = "differential", eselist = eselist), {
    session$setInputs(
      "expression-assay" = "counts",
      "expression-experiment" = "counts",
      "expression-selectmatrix-obs" = 60,
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all"
    )

    rendered <- output$differentialtable
    expect_type(rendered, "list")
    expect_match(rendered[[1]], "Differential expression in assay: counts")
  })
})
