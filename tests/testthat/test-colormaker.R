# make_color_scale()

test_that("make_color_scale returns the requested number of colors for a mid-range value", {
  cols <- make_color_scale(5)

  expect_length(cols, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", cols)))
})

test_that("make_color_scale draws from the start of the fixed colour-blind-safe palette", {
  cols <- make_color_scale(5)

  expect_equal(cols, COLORBLIND_PALETTE[1:5])
})

test_that("make_color_scale returns colours stably for the same count", {
  expect_equal(make_color_scale(3), make_color_scale(3))
  expect_equal(make_color_scale(8), COLORBLIND_PALETTE)
})

test_that("make_color_scale defaults to the colour-blind palette but honours a named RColorBrewer palette", {
  expect_equal(make_color_scale(4, palette = "colorblind"), make_color_scale(4))

  brewer <- make_color_scale(4, palette = "Set1")
  expect_length(brewer, 4)
  expect_false(identical(brewer, make_color_scale(4)))
})

test_that("make_color_scale still returns the requested count when ncolors is below 3", {
  cols1 <- make_color_scale(1)
  cols2 <- make_color_scale(2)

  expect_equal(cols1, COLORBLIND_PALETTE[1])
  expect_equal(cols2, COLORBLIND_PALETTE[1:2])
})

test_that("make_color_scale interpolates and warns when ncolors exceeds the base palette", {
  expect_message(cols <- make_color_scale(20), "more than the")

  expect_length(cols, 20)
  expect_equal(length(unique(cols)), 20)
})

test_that("make_color_scale uses an RColorBrewer palette's own low-n colours below 3, not an interpolated subset", {
  cols <- make_color_scale(2, palette = "Set1")

  expect_equal(sort(cols), sort(RColorBrewer::brewer.pal(3, "Set1")[1:2]))
  expect_false("#FF7F00" %in% cols) # the muddled orange colorRampPalette() used to produce here
})
