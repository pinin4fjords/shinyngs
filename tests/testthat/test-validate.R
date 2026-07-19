# validate_or_catch()

test_that("validate_or_catch returns the value of a successful expression", {
  expect_equal(validate_or_catch(1 + 1), 2)
})

test_that("validate_or_catch converts an error into a shiny.silent.error carrying the original message", {
  err <- tryCatch(
    validate_or_catch(stop("boom")),
    error = function(e) e
  )

  expect_s3_class(err, "shiny.silent.error")
  expect_equal(conditionMessage(err), "boom")
})

# validate_indices()

test_that("validate_indices accepts a comma-separated list of integer indices", {
  assay_data <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3), c = matrix(1:6, ncol = 2))

  expect_equal(validate_indices(assay_data, "1,2"), c(1, 2))
})

test_that("validate_indices accepts a comma-separated list of assay names, prettified to match", {
  # prettify_names defaults to TRUE, so the supplied names must be matched
  # against assay names in their prettified form
  assay_data <- list(A = matrix(1:9, ncol = 3), B = matrix(1:12, ncol = 3), C = matrix(1:6, ncol = 2))

  expect_equal(validate_indices(assay_data, "a,b"), c("A", "B"))
})

test_that("validate_indices can skip name prettification", {
  assay_data <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3))

  expect_equal(validate_indices(assay_data, "a,b", prettify_names = FALSE), c("a", "b"))
})

test_that("validate_indices errors on an invalid assay name", {
  assay_data <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3))

  expect_error(validate_indices(assay_data, "a,nonsense", prettify_names = FALSE), "Invalid assays")
})

test_that("validate_indices errors on an out-of-range integer index", {
  assay_data <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3))

  expect_error(validate_indices(assay_data, "1,5"), "Invalid assays")
})

test_that("validate_indices can invert a name-based selection", {
  assay_data <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3), c = matrix(1:6, ncol = 2))

  expect_equal(validate_indices(assay_data, "a,b", invert_assays = TRUE, prettify_names = FALSE), "c")
})

test_that("validate_indices can invert an integer-index selection", {
  assay_data <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3), c = matrix(1:6, ncol = 2))

  expect_equal(validate_indices(assay_data, "1,2", invert_assays = TRUE), 3)
})
