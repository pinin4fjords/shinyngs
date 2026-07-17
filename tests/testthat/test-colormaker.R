# makeColorScale()

test_that("makeColorScale returns the requested number of colors for a mid-range value", {
  cols <- makeColorScale(5, palette = "Set1")

  expect_length(cols, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", cols)))
})

test_that("makeColorScale interpolates when ncolors exceeds the palette maximum", {
  cols <- makeColorScale(20, palette = "Set1")

  expect_length(cols, 20)
  expect_equal(length(unique(cols)), 20)
})

test_that("makeColorScale still returns the requested count when ncolors is below 3", {
  cols1 <- makeColorScale(1, palette = "Set1")
  cols2 <- makeColorScale(2, palette = "Set1")

  expect_length(cols1, 1)
  expect_length(cols2, 2)

  ramp <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(3)
  expect_equal(cols1, rev(ramp[1]))
  expect_equal(cols2, rev(ramp[1:2]))
})

test_that("makeColorScale reverses the underlying palette order", {
  cols <- makeColorScale(3, palette = "Set1")
  expected <- rev(RColorBrewer::brewer.pal(3, "Set1"))

  expect_equal(cols, expected)
})
