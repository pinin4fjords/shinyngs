# URL bookmarking round-trip
#
# Runs from an on-disk app.R (see shinytest2_bookmark_app_driver) so bookmarking
# is enabled at session construction, as in real use. Restored values are read
# from the DOM: updateNumericInput() etc. set the control's value on restore but
# the client input registry only reflects it once the input reports.
#
# Split into its own file (rather than living alongside the other rnaseq
# AppDriver tests) since its 40s timeouts and separate on-disk-app process
# make it the single heaviest shinytest2 test; giving it its own file lets
# testthat's parallel workers pick it up independently of the others.

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
