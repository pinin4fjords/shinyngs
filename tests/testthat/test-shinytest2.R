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
  expect_true("rnaseq-pca-pca-selectmatrix-geneSelect" %in% outputs)
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
  expect_true("rnaseq-heatmap-clustering-heatmap-selectmatrix-geneSelect" %in% outputs)
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
  Sys.sleep(2)

  app$click("shinyngs_share_view")
  Sys.sleep(3)
  url <- app$get_js("window.location.href")

  expect_match(url, "\\?", info = "bookmark should put state in the address bar")
  expect_true(grepl("contrast_filtersets", utils::URLdecode(url)))
  expect_false(grepl("plotly_", url), info = "transient plotly inputs are excluded")
  expect_false(grepl("_rows_", url), info = "transient DataTable inputs are excluded")

  app$run_js(sprintf("window.location.href = %s;", jsonlite::toJSON(url, auto_unbox = TRUE)))

  restored <- NA_character_
  for (i in seq_len(20)) {
    Sys.sleep(2)
    val <- app$get_js("(function(){var e=document.getElementById('rnaseq-differential-differential-fold_change0'); return e ? String(e.value) : 'NOEL';})()")
    if (!is.null(val) && val == "7") {
      restored <- val
      break
    }
  }
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
    Sys.sleep(0.5)

    rendered <- output$differentialtable
    expect_type(rendered, "list")
    expect_match(rendered[[1]], "Differential expression in assay: counts")
  })
})
