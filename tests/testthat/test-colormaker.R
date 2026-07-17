# makeColorScale()

test_that("makeColorScale returns the requested number of colors for a mid-range value", {
  cols <- makeColorScale(5)

  expect_length(cols, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", cols)))
})

test_that("makeColorScale draws from the start of the fixed colour-blind-safe palette", {
  cols <- makeColorScale(5)

  expect_equal(cols, COLORBLIND_PALETTE[1:5])
})

test_that("makeColorScale returns colours stably for the same count", {
  expect_equal(makeColorScale(3), makeColorScale(3))
  expect_equal(makeColorScale(8), COLORBLIND_PALETTE)
})

test_that("makeColorScale defaults to the colour-blind palette but honours a named RColorBrewer palette", {
  expect_equal(makeColorScale(4, palette = "colorblind"), makeColorScale(4))

  brewer <- makeColorScale(4, palette = "Set1")
  expect_length(brewer, 4)
  expect_false(identical(brewer, makeColorScale(4)))
})

test_that("makeColorScale still returns the requested count when ncolors is below 3", {
  cols1 <- makeColorScale(1)
  cols2 <- makeColorScale(2)

  expect_equal(cols1, COLORBLIND_PALETTE[1])
  expect_equal(cols2, COLORBLIND_PALETTE[1:2])
})

test_that("makeColorScale interpolates and warns when ncolors exceeds the base palette", {
  expect_message(cols <- makeColorScale(20), "more than the")

  expect_length(cols, 20)
  expect_equal(length(unique(cols)), 20)
})
