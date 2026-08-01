# fold_change()

test_that("fold_change returns 1 where the two vectors are equal", {
  expect_equal(fold_change(c(2, 2, 2), c(2, 2, 2)), c(1, 1, 1))
})

test_that("fold_change returns a plain ratio when vec2 exceeds vec1", {
  expect_equal(fold_change(c(2, 2, 2), c(2, 4, 8)), c(1, 2, 4))
})

test_that("fold_change returns a negative reciprocal when vec2 is smaller than vec1", {
  expect_equal(fold_change(c(4, 8), c(2, 2)), c(-2, -4))
})

test_that("fold_change handles a mix of increases, decreases and no change", {
  vec1 <- c(10, 10, 10)
  vec2 <- c(10, 20, 5)

  expect_equal(fold_change(vec1, vec2), c(1, 2, -2))
})

test_that("base contrast tables do not force the selected expression subset", {
  selected_matrix_calls <- 0L
  ese <- make_medium_module_eselist()[[1]]
  summaries <- list(condition = cbind(ctrl = seq_len(nrow(ese)), treated = seq_len(nrow(ese)) + 1))
  rownames(summaries$condition) <- rownames(ese)
  contrast <- list("1" = list(Variable = "condition", Group.1 = "ctrl", Group.2 = "treated"))

  selectmatrix_reactives <- list(
    selectMatrix = function() {
      selected_matrix_calls <<- selected_matrix_calls + 1L
      matrix(0, nrow = 1, dimnames = list(rownames(ese)[1], "s1"))
    },
    getExperiment = function() ese,
    getAssay = function() "counts"
  )

  shiny::testServer(function(input, output, session) {
    tables <- contrastTableBuilder(
      selectmatrix_reactives,
      getSummaries = function() summaries,
      getAllContrasts = function() contrast,
      getAllContrastsNumbers = function() c("condition: treated vs ctrl" = "1"),
      fcsAvailable = function() FALSE,
      pvalsAvailable = function() FALSE,
      qvalsAvailable = function() FALSE
    )
  }, {
    expect_equal(nrow(shiny::isolate(tables$contrastsTables()[[1]])), nrow(ese))
  })
  expect_equal(selected_matrix_calls, 0L)
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
