# withHelpIcon()

test_that("withHelpIcon returns the label unchanged when no tooltip is given", {
  expect_equal(withHelpIcon("Some label"), "Some label")
})

test_that("withHelpIcon attaches a tooltip-triggering icon when tooltip is given", {
  result <- withHelpIcon("Some label", "Explains the field")
  html <- as.character(result)

  expect_true(grepl("Some label", html, fixed = TRUE))
  expect_true(grepl("circle-info", html, fixed = TRUE))
  expect_true(grepl("Explains the field", html, fixed = TRUE))
})

# inlineField()

test_that("inlineField wraps a field definition with an inline bold label", {
  result <- inlineField(shiny::numericInput("foo", label = NULL, value = 50), "FOO")
  html <- as.character(result)

  expect_true(grepl("<b>FOO:</b>", html, fixed = TRUE))
  expect_true(grepl("id=\"foo\"", html, fixed = TRUE))
})

test_that("inlineField passes a tooltip through to the label", {
  result <- inlineField(shiny::numericInput("foo", label = NULL, value = 50), "FOO", tooltip = "A tooltip")
  html <- as.character(result)

  expect_true(grepl("A tooltip", html, fixed = TRUE))
})

# a11yControl()

test_that("a11yControl sets an aria-label and wraps the tag in a tooltip", {
  result <- a11yControl(shiny::actionButton("btn", "Click"), label = "Click the button")
  html <- as.character(result)

  expect_true(grepl('aria-label="Click the button"', html, fixed = TRUE))
})

test_that("a11yControl defaults the tooltip text to the label", {
  with_label_only <- as.character(a11yControl(shiny::actionButton("btn", "Click"), label = "Click the button"))
  with_explicit_tooltip <- as.character(a11yControl(shiny::actionButton("btn", "Click"), label = "Click the button", tooltip = "Click the button"))

  expect_equal(with_label_only, with_explicit_tooltip)
})

# cardinalNumericField()

test_that("cardinalNumericField builds a labelled numeric field with a cardinality selector", {
  result <- cardinalNumericField("val", "val_cardinality", "Fold change", value = 2, cardinality = ">=")
  html <- as.character(result)

  expect_true(grepl("<b>Fold change:</b>", html, fixed = TRUE))
  expect_true(grepl('id="val"', html, fixed = TRUE))
  expect_true(grepl('id="val_cardinality"', html, fixed = TRUE))
  expect_true(grepl("shinyngs-cardinalfield", html, fixed = TRUE))
})

# evaluateCardinalFilter()

test_that("evaluateCardinalFilter applies '<=' and '>='", {
  values <- c(-5, -1, 0, 1, 5, NA)

  expect_equal(evaluateCardinalFilter(values, "<=", 1), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(evaluateCardinalFilter(values, ">=", 1), c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})

test_that("evaluateCardinalFilter applies the symmetric '>= or <= -' cardinality", {
  values <- c(-5, -1, 0, 1, 5, NA)

  expect_equal(evaluateCardinalFilter(values, ">= or <= -", 2), c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("evaluateCardinalFilter applies the symmetric '<= and >= -' cardinality", {
  values <- c(-5, -1, 0, 1, 5, NA)

  expect_equal(evaluateCardinalFilter(values, "<= and >= -", 2), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("evaluateCardinalFilter errors on an unrecognised cardinality", {
  expect_error(evaluateCardinalFilter(1:3, "nonsense", 1), "invalid cardinality")
})
