# chipseq shares prepare_app()'s multi-panel branch with rnaseq, so a boot +
# one composed-module check is enough to confirm the branch wires up
# correctly for this platform label and homeTab()/PCA composition, without
# re-testing every rnaseq-covered panel (see test-shinytest2-rnaseq.R)

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
