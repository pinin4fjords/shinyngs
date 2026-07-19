# tricubeMovingAverage()

test_that("tricubeMovingAverage returns a smoothed vector of the same length", {
  set.seed(1)
  x <- rnorm(50)
  smoothed <- tricubeMovingAverage(x, span = 0.5)

  expect_length(smoothed, length(x))
  expect_true(all(is.finite(smoothed)))
})

test_that("tricubeMovingAverage smooths a step function towards its local average", {
  x <- c(rep(0, 25), rep(10, 25))
  smoothed <- tricubeMovingAverage(x, span = 0.3)

  # points well inside a flat run stay at the flat value
  expect_equal(smoothed[5], 0)
  expect_equal(smoothed[45], 10)

  # points straddling the step are pulled towards an intermediate value
  expect_true(smoothed[25] > 0 && smoothed[25] < 10)
})

test_that("tricubeMovingAverage returns x unchanged for span <= 0", {
  x <- rnorm(10)
  expect_equal(tricubeMovingAverage(x, span = 0), x)
})

test_that("tricubeMovingAverage clamps span above 1", {
  x <- rnorm(20)
  expect_equal(tricubeMovingAverage(x, span = 2), tricubeMovingAverage(x, span = 1))
})

# quantileOfSorted()

test_that("quantileOfSorted matches quantile() type 7 on an already-sorted vector", {
  x <- sort(c(3, 1, 4, 1, 5, 9, 2, 6))
  probs <- c(0, 0.25, 0.5, 0.75, 1)

  expect_equal(quantileOfSorted(x, probs), unname(quantile(x, probs, type = 7)))
})

test_that("quantileOfSorted returns the endpoints for probs 0 and 1", {
  x <- sort(c(10, 2, 7, 4))
  expect_equal(quantileOfSorted(x, 0), x[1])
  expect_equal(quantileOfSorted(x, 1), x[length(x)])
})

# plotly_barcodeplot()

test_that("plotly_barcodeplot draws a rug-tick trace and an enrichment worm curve", {
  set.seed(1)
  gene_ids <- paste0("gene", 1:100)
  fold_changes <- rnorm(100)
  set_gene_ids <- sample(gene_ids, 15)

  built <- plotly::plotly_build(plotly_barcodeplot(fold_changes, gene_ids, set_gene_ids))

  trace_names <- vapply(built$x$data, function(t) as.character(t$name), character(1))
  expect_setequal(trace_names, c("Gene set", "Enrichment"))
})

test_that("plotly_barcodeplot handles no gene set members found in the ranked list", {
  gene_ids <- paste0("gene", 1:20)
  fold_changes <- rnorm(20)

  p <- suppressWarnings(plotly_barcodeplot(fold_changes, gene_ids, set_gene_ids = "not_present"))
  built <- suppressWarnings(plotly::plotly_build(p))

  trace_names <- vapply(built$x$data, function(t) if (is.null(t$name)) NA_character_ else as.character(t$name), character(1))
  expect_false(any(trace_names %in% c("Gene set", "Enrichment")))
  annotation_texts <- vapply(built$x$layout$annotations, function(a) a$text, character(1))
  expect_true(any(grepl("No genes from this set", annotation_texts)))
})

test_that("plotly_barcodeplot drops NA fold changes before ranking", {
  gene_ids <- paste0("gene", 1:10)
  fold_changes <- c(rnorm(9), NA)
  set_gene_ids <- gene_ids[1:3]

  built <- plotly::plotly_build(plotly_barcodeplot(fold_changes, gene_ids, set_gene_ids))

  # x axis range covers only the 9 non-NA genes
  expect_equal(built$x$layout$xaxis$range, c(0.5, 9.5))
})

test_that("plotly_barcodeplot converts newlines in the title to <br> tags", {
  gene_ids <- paste0("gene", 1:10)
  fold_changes <- rnorm(10)

  built <- plotly::plotly_build(plotly_barcodeplot(fold_changes, gene_ids, set_gene_ids = gene_ids[1], plot_title = "line one\nline two"))

  expect_equal(built$x$layout$title, "line one<br>line two")
})
