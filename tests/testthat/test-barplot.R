# plotly_barchart()

test_that("plotly_barchart draws one bar trace per matrix row, stacked by default", {
  m <- matrix(1:6, nrow = 2, byrow = TRUE, dimnames = list(c("up", "down"), c("s1", "s2", "s3")))

  built <- plotly::plotly_build(plotly_barchart(m, barmode = "stack", ylab = "Count"))

  expect_equal(built$x$layout$barmode, "stack")
  expect_equal(built$x$layout$yaxis$title, "Count")
  expect_length(built$x$data, 2)
  expect_true(all(vapply(built$x$data, function(t) t$type, character(1)) == "bar"))
})

test_that("plotly_barchart reorders rows by decreasing mean when barmode is overlay", {
  m <- matrix(c(1, 1, 1, 10, 10, 10), nrow = 2, byrow = TRUE, dimnames = list(c("low", "high"), c("s1", "s2", "s3")))

  built <- plotly::plotly_build(plotly_barchart(m, barmode = "overlay"))

  trace_names <- vapply(built$x$data, function(t) as.character(t$name), character(1))
  expect_equal(trace_names[1], "high")
})

test_that("plotly_barchart keeps column names as characters, not coerced to numbers", {
  m <- matrix(1:4, nrow = 2, dimnames = list(c("a", "b"), c("2020", "2021")))

  built <- plotly::plotly_build(plotly_barchart(m))

  expect_type(built$x$data[[1]]$x, "character")
})
