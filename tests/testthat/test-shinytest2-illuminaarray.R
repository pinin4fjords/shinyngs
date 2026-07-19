# illuminaarray shares prepareApp()'s multi-panel branch with rnaseq, so a
# boot + one composed-module check is enough to confirm the branch wires up
# correctly for this platform label and homeTab()/PCA composition, without
# re-testing every rnaseq-covered panel (see test-shinytest2-rnaseq.R)

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
