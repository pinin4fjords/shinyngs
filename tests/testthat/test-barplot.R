# plotly_barplot()

test_that("plotly_barplot draws one trace per row, with columns along x", {
  m <- matrix(c(3, 1, 2, 4), nrow = 2, dimnames = list(c("a", "b"), c("x", "y")))
  built <- plotly::plotly_build(plotly_barplot(m))

  expect_length(built$x$data, 2)
  expect_equal(as.character(built$x$data[[1]]$x), c("x", "y"))
  expect_equal(as.numeric(built$x$data[[1]]$y), c(3, 2))
  expect_equal(as.numeric(built$x$data[[2]]$y), c(1, 4))
})

test_that("plotly_barplot re-orders rows by descending mean in overlay mode", {
  m <- matrix(c(1, 2, 9, 20), nrow = 2, byrow = TRUE, dimnames = list(c("low", "high"), c("x", "y")))
  built <- plotly::plotly_build(plotly_barplot(m, barmode = "overlay"))

  expect_equal(built$x$data[[1]]$name, "high")
  expect_equal(as.numeric(built$x$data[[1]]$y), c(9, 20))
})

test_that("plotly_barplot hides the legend for a single-row matrix", {
  m <- matrix(c(3, 2), nrow = 1, dimnames = list("Count", c("x", "y")))
  p <- plotly_barplot(m)

  expect_false(p$x$layoutAttrs[[1]]$showlegend)
})

test_that("plotly_barplot pins the x axis to plotmatrix's column order, not alphabetical", {
  m <- matrix(c(3, 2, 1), nrow = 1, dimnames = list("Count", c("zebra", "mango", "apple")))
  built <- plotly::plotly_build(plotly_barplot(m))

  expect_equal(built$x$layout$xaxis$categoryorder, "array")
  expect_equal(built$x$layout$xaxis$categoryarray, c("zebra", "mango", "apple"))
})

# countMatrixByCategory()

test_that("countMatrixByCategory tallies a single category column, ordered by descending count", {
  annotation <- data.frame(biotype = c("lncRNA", "protein_coding", "protein_coding", "protein_coding"))
  m <- countMatrixByCategory(annotation, "biotype")

  expect_equal(dim(m), c(1, 2))
  expect_equal(colnames(m), c("protein_coding", "lncRNA"))
  expect_equal(as.numeric(m), c(3, 1))
})

test_that("countMatrixByCategory tallies a category split by a second column", {
  annotation <- data.frame(
    biotype = c("protein_coding", "protein_coding", "lncRNA"),
    direction = c("Up", "Down", "Up")
  )
  m <- countMatrixByCategory(annotation, "biotype", "direction")

  expect_equal(sort(rownames(m)), c("Down", "Up"))
  expect_equal(sort(colnames(m)), c("lncRNA", "protein_coding"))
  expect_equal(m["Up", "protein_coding"], 1)
  expect_equal(m["Down", "protein_coding"], 1)
  expect_equal(m["Up", "lncRNA"], 1)
})

# plotly_count_barplot()

test_that("plotly_count_barplot counts rows by a single category", {
  annotation <- data.frame(biotype = c("lncRNA", "protein_coding", "protein_coding", "protein_coding"))
  built <- plotly::plotly_build(plotly_count_barplot(annotation, "biotype"))

  expect_length(built$x$data, 1)
  expect_equal(as.character(built$x$data[[1]]$x), c("protein_coding", "lncRNA"))
  expect_equal(as.numeric(built$x$data[[1]]$y), c(3, 1))
})

test_that("plotly_count_barplot splits counts by a fill column into separate traces", {
  annotation <- data.frame(
    biotype = c("protein_coding", "protein_coding", "lncRNA"),
    direction = c("Up", "Down", "Up")
  )
  built <- plotly::plotly_build(plotly_count_barplot(annotation, "biotype", fill = "direction"))

  expect_length(built$x$data, 2)
})

test_that("plotly_count_barplot errors for an unknown column", {
  annotation <- data.frame(biotype = c("lncRNA", "protein_coding"))

  expect_error(plotly_count_barplot(annotation, "nonexistent"), "not a column")
  expect_error(plotly_count_barplot(annotation, "biotype", fill = "nonexistent"), "not a column")
})

test_that("plotly_count_barplot errors informatively for an all-missing category", {
  annotation <- data.frame(biotype = c("lncRNA", "protein_coding"), phase = NA_character_)

  expect_error(plotly_count_barplot(annotation, "phase"), "no non-missing values")
})
