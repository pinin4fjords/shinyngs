# ggplotify()

test_that("ggplotify computes a density trace when a sample has zero values", {
  # rmzeros turns zeros into NA ahead of the log2 transform, so a sample
  # with any zero entries reproduces the crash from issue #249 unless
  # those NAs are dropped before stats::density() is called.
  mat <- matrix(
    c(0, 10, 100, 1000, 5, 50, 500, 5000),
    nrow = 4, dimnames = list(paste0("gene", 1:4), c("s1", "s2"))
  )
  experiment <- data.frame(row.names = c("s1", "s2"))

  plotdata <- ggplotify(mat, experiment, value_type = "density")

  expect_setequal(unique(plotdata$name), c("s1", "s2"))
  expect_true(all(is.finite(plotdata$density)))
})
