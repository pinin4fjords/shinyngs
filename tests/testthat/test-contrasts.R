# foldChange()

test_that("foldChange returns 1 where the two vectors are equal", {
  expect_equal(foldChange(c(2, 2, 2), c(2, 2, 2)), c(1, 1, 1))
})

test_that("foldChange returns a plain ratio when vec2 exceeds vec1", {
  expect_equal(foldChange(c(2, 2, 2), c(2, 4, 8)), c(1, 2, 4))
})

test_that("foldChange returns a negative reciprocal when vec2 is smaller than vec1", {
  expect_equal(foldChange(c(4, 8), c(2, 2)), c(-2, -4))
})

test_that("foldChange handles a mix of increases, decreases and no change", {
  vec1 <- c(10, 10, 10)
  vec2 <- c(10, 20, 5)

  expect_equal(foldChange(vec1, vec2), c(1, 2, -2))
})

# contrastSelection()$getSelectedContrastSamples()

test_that("getSelectedContrastSamples resolves samples for the selected contrast(s), nested per filter set", {
  contrast_samples <- list(
    "1" = list(c("s1", "s2"), c("s3", "s4")),
    "2" = list(c("s5", "s6"), c("s7", "s8"))
  )

  selection <- contrastSelection(
    getSelectedContrastNumbers = function() list("1"),
    getAllContrasts = function() NULL,
    getContrastSamples = function() contrast_samples,
    makeContrastNames = function() NULL,
    makeSafeContrastNames = function() NULL
  )

  # Single filter set, single contrast: matches the getSelectedContrasts()[[1]][[1]] nesting used elsewhere
  result <- shiny::isolate(selection$getSelectedContrastSamples())
  expect_equal(result[[1]][[1]], list(c("s1", "s2"), c("s3", "s4")))
})

test_that("getSelectedContrastSamples handles multiple contrasts within a filter set", {
  contrast_samples <- list(
    "1" = list(c("s1", "s2"), c("s3", "s4")),
    "2" = list(c("s5", "s6"), c("s7", "s8"))
  )

  selection <- contrastSelection(
    getSelectedContrastNumbers = function() list(c("1", "2")),
    getAllContrasts = function() NULL,
    getContrastSamples = function() contrast_samples,
    makeContrastNames = function() NULL,
    makeSafeContrastNames = function() NULL
  )

  result <- shiny::isolate(selection$getSelectedContrastSamples())
  expect_equal(result[[1]][["1"]], list(c("s1", "s2"), c("s3", "s4")))
  expect_equal(result[[1]][["2"]], list(c("s5", "s6"), c("s7", "s8")))
})
