test_that("interactive_differential_summary plots up and down counts around zero", {
  summary <- data.frame(
    Variable = c("condition", "condition"),
    `group 1` = c("control", "control"),
    `group 2` = c("treated_a", "treated_b"),
    `Differential genes (up)` = c(12, 0),
    `Differential genes (down)` = c(4, 0),
    check.names = FALSE
  )
  built <- plotly::plotly_build(interactive_differential_summary(summary))

  expect_equal(as.numeric(built$x$data[[1]]$x), c(-4, 0))
  expect_equal(as.numeric(built$x$data[[2]]$x), c(12, 0))
  expect_true(all(vapply(built$x$data, function(trace) all(trace$textposition == "none"), logical(1))))
  expect_equal(built$x$layout$barmode, "relative")
})

test_that("direction counts ignore neutral and missing fold changes", {
  expect_equal(
    count_differential_directions(c(4, -4, 1, -1, 0, NA_real_)),
    c(up = 1, down = 1)
  )
})
