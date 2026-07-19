# static_scatterplot()

test_that("static_scatterplot colors every colorby category, not just labelled points", {
  colorby <- c(FALSE, FALSE, FALSE, TRUE)
  labels <- rep(NA_character_, length(colorby))
  labels[colorby] <- "significant"

  p <- static_scatterplot(
    x = c(1, 2, 3, 4),
    y = c(1, 2, 3, 4),
    colorby = colorby,
    labels = labels,
    show_labels = TRUE
  )

  built <- ggplot2::ggplot_build(p)
  expect_length(unique(built$data[[1]]$colour), 2)
})

# interactive_scatterplot() / drawLines()

test_that("threshold lines span the full plotted axis range instead of stopping at the data extremes", {
  x <- c(-4, -1, 0, 1, 3)
  y <- c(-2, 0, 1, 2, 4)

  lines <- data.frame(
    name = c("hline", "hline", "vline", "vline"),
    x = c(min(x), max(x), 0, 0),
    y = c(0, 0, min(y), max(y))
  )

  p <- interactive_scatterplot(x = x, y = y, lines = lines)
  built <- plotly::plotly_build(p)

  xaxis_range <- built$x$layout$xaxis$range
  yaxis_range <- built$x$layout$yaxis$range

  hline_trace <- Filter(function(tr) identical(tr$name, "hline"), built$x$data)[[1]]
  vline_trace <- Filter(function(tr) identical(tr$name, "vline"), built$x$data)[[1]]

  expect_equal(sort(hline_trace$x), sort(xaxis_range))
  expect_equal(sort(vline_trace$y), sort(yaxis_range))
})

test_that("3D scatter labels each scene axis with its own title", {
  p <- interactive_scatterplot(
    x = c(1, 2, 3), y = c(4, 5, 6), z = c(7, 8, 9),
    plot_type = "scatter3d", xlab = "xtitle", ylab = "ytitle", zlab = "ztitle"
  )
  scene <- p$x$layoutAttrs[[1]]$scene

  expect_equal(scene$xaxis$title, "xtitle")
  expect_equal(scene$yaxis$title, "ytitle")
  expect_equal(scene$zaxis$title, "ztitle")
})

test_that("xrange/yrange override the auto-computed axis range and the extent of threshold lines", {
  x <- c(-4, -1, 0, 1, 3)
  y <- c(-2, 0, 1, 2, 4)

  p <- interactive_scatterplot(
    x = x, y = y,
    hline_thresholds = list(threshold = 0),
    xrange = c(-10, 10),
    yrange = c(-5, 5)
  )
  built <- plotly::plotly_build(p)

  expect_equal(built$x$layout$xaxis$range, c(-10, 10))
  expect_equal(built$x$layout$yaxis$range, c(-5, 5))

  hline_trace <- Filter(function(tr) identical(tr$name, "threshold"), built$x$data)[[1]]
  expect_equal(sort(hline_trace$x), c(-10, 10))
})

# interactive_scatterplot() colorby_menu

test_that("colorby_menu adds a dropdown and shows only the first option's points initially", {
  x <- c(1, 2, 3, 4)
  y <- c(4, 3, 2, 1)
  colorby_menu <- list(
    Group = c("A", "A", "B", "B"),
    Batch = c("1", "2", "1", "2")
  )

  p <- interactive_scatterplot(x = x, y = y, colorby_menu = colorby_menu)
  built <- plotly::plotly_build(p)

  updatemenus <- built$x$layout$updatemenus
  expect_length(updatemenus, 1)

  button_labels <- vapply(updatemenus[[1]]$buttons, function(b) b$label, character(1))
  expect_equal(button_labels, c("Group", "Batch"))

  trace_names <- vapply(built$x$data, function(tr) tr$name, character(1))
  visible <- vapply(built$x$data, function(tr) isTRUE(tr$visible), logical(1))

  expect_equal(sum(visible), 2)
  expect_true(all(trace_names[visible] %in% c("A", "B")))
  expect_true(all(!(trace_names[!visible] %in% c("A", "B"))))
})

test_that("each colorby_menu option keeps its own palette instead of bleeding into other options' colours", {
  # plotly shares one discrete colour domain across every trace on a widget,
  # so options with independently-sized palettes can blend together unless
  # each trace's marker colour is set explicitly rather than left to plotly's
  # automatic discrete colour mapping.
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(8, 7, 6, 5, 4, 3, 2, 1)
  colorby_menu <- list(
    Group = rep(c("A", "B"), 4),
    Batch = rep(c("X", "Y", "Z"), length.out = 8)
  )

  p <- interactive_scatterplot(x = x, y = y, colorby_menu = colorby_menu)
  built <- plotly::plotly_build(p)

  color_by_name <- setNames(
    vapply(built$x$data, function(tr) tr$marker$color[1], character(1)),
    vapply(built$x$data, function(tr) tr$name, character(1))
  )

  two_colors <- make_color_scale(2)
  three_colors <- make_color_scale(3)

  expect_equal(unname(color_by_name["A"]), two_colors[1])
  expect_equal(unname(color_by_name["B"]), two_colors[2])
  expect_equal(unname(color_by_name["X"]), three_colors[1])
  expect_equal(unname(color_by_name["Y"]), three_colors[2])
  expect_equal(unname(color_by_name["Z"]), three_colors[3])
})

test_that("colorby_menu is opt-in: omitting it leaves the plot without any updatemenus", {
  p <- interactive_scatterplot(x = c(1, 2, 3), y = c(3, 2, 1), colorby = c("A", "B", "A"))
  built <- plotly::plotly_build(p)

  expect_null(built$x$layout$updatemenus)
})
