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
