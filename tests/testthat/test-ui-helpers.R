# configureBookmarking() is not covered here: it calls shiny::enableBookmarking()
# unconditionally, which walks the session's ancestor chain looking for a real
# ShinySession and errors ("ShinySession not found") under shiny::testServer()'s
# MockShinySession, regardless of what the rest of the function does. Exercising
# it would require a real browser-backed session (shinytest2), which is out of
# scope for this test file.

# bookmarkedInputValue()

test_that("bookmarkedInputValue reads the namespaced key first", {
  state <- list(input = list("mod-label" = "namespaced", "label" = "bare"))
  session <- list(ns = function(id) paste0("mod-", id))

  expect_equal(bookmarkedInputValue(state, session, "label"), "namespaced")
})

test_that("bookmarkedInputValue falls back to the bare id when the namespaced key is absent", {
  state <- list(input = list("label" = "bare"))
  session <- list(ns = function(id) paste0("mod-", id))

  expect_equal(bookmarkedInputValue(state, session, "label"), "bare")
})

test_that("bookmarkedInputValue returns NULL when neither key is present", {
  state <- list(input = list())
  session <- list(ns = function(id) paste0("mod-", id))

  expect_null(bookmarkedInputValue(state, session, "label"))
})

# moduleLayout()

test_that("moduleLayout places controls in the sidebar and main content in the body", {
  layout <- moduleLayout(controls = tags$div(id = "the-controls"), main = tags$div(id = "the-main"))

  rendered <- as.character(layout)
  expect_match(rendered, "the-controls", fixed = TRUE)
  expect_match(rendered, "the-main", fixed = TRUE)
})

test_that("moduleLayout's sidebar defaults to 320px wide", {
  layout <- moduleLayout(controls = tags$div("controls"), main = tags$div("main"))

  expect_match(as.character(layout), "320px", fixed = TRUE)
})

# moduleMain()

test_that("moduleMain renders a section title before the body content", {
  main <- moduleMain("My Section", tags$p("body content"))

  rendered <- as.character(main)
  expect_match(rendered, "<h3", fixed = TRUE)
  expect_match(rendered, "My Section", fixed = TRUE)
  expect_match(rendered, "body content", fixed = TRUE)
})

test_that("moduleMain omits the heading entirely when title is NULL", {
  main <- moduleMain(NULL, tags$p("body content"))

  expect_no_match(as.character(main), "<h3", fixed = TRUE)
})

test_that("moduleMain includes the help trigger when supplied", {
  main <- moduleMain("Title", tags$p("body"), help = tags$a(id = "help-link", "help"))

  expect_match(as.character(main), "help-link", fixed = TRUE)
})

# shinyngsSpinnerColor()

test_that("shinyngsSpinnerColor returns the brand accent hex colour", {
  expect_equal(shinyngsSpinnerColor(), SHINYNGS_ACCENT)
  expect_match(shinyngsSpinnerColor(), "^#[0-9a-f]{6}$")
})

# shinyngsPlotlyConfig()

test_that("shinyngsPlotlyConfig drops the plotly logo and the noisier selection buttons", {
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")

  configured <- shinyngsPlotlyConfig(p, filename = "my_plot", format = "svg")

  expect_false(configured$x$config$displaylogo)
  expect_true(all(c("sendDataToCloud", "lasso2d", "select2d") %in% configured$x$config$modeBarButtonsToRemove))
})

test_that("shinyngsPlotlyConfig names the downloaded image after the plot, in the requested format", {
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")

  configured <- shinyngsPlotlyConfig(p, filename = "my_plot", format = "svg")

  expect_equal(configured$x$config$toImageButtonOptions$filename, "my_plot")
  expect_equal(configured$x$config$toImageButtonOptions$format, "svg")
})

test_that("shinyngsPlotlyConfig defaults to a generic filename and PNG format", {
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")

  configured <- shinyngsPlotlyConfig(p)

  expect_equal(configured$x$config$toImageButtonOptions$filename, "shinyngs_plot")
  expect_equal(configured$x$config$toImageButtonOptions$format, "png")
})

# fieldSets()

test_that("fieldSets renders one panel per named element, titled with a prettified name", {
  fs <- fieldSets("myfields", list(select_assay_data = tags$div(id = "assay-controls"), export = tags$div(id = "export-controls")))

  rendered <- as.character(fs)
  expect_match(rendered, "Select assay data", fixed = TRUE)
  expect_match(rendered, "Export", fixed = TRUE)
  expect_match(rendered, "assay-controls", fixed = TRUE)
  expect_match(rendered, "export-controls", fixed = TRUE)
})

test_that("fieldSets' panels are all open by default", {
  fs <- fieldSets("myfields", list(a = tags$div("A"), b = tags$div("B")))

  rendered <- as.character(fs)
  expect_equal(lengths(regmatches(rendered, gregexpr("accordion-collapse collapse show", rendered))), 2)
})
