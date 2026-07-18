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

# plotly_scatterplot() / drawLines()

test_that("threshold lines span the full plotted axis range instead of stopping at the data extremes", {
  x <- c(-4, -1, 0, 1, 3)
  y <- c(-2, 0, 1, 2, 4)

  lines <- data.frame(
    name = c("hline", "hline", "vline", "vline"),
    x = c(min(x), max(x), 0, 0),
    y = c(0, 0, min(y), max(y))
  )

  p <- plotly_scatterplot(x = x, y = y, lines = lines)
  built <- plotly::plotly_build(p)

  xaxis_range <- built$x$layout$xaxis$range
  yaxis_range <- built$x$layout$yaxis$range

  hline_trace <- Filter(function(tr) identical(tr$name, "hline"), built$x$data)[[1]]
  vline_trace <- Filter(function(tr) identical(tr$name, "vline"), built$x$data)[[1]]

  expect_equal(sort(hline_trace$x), sort(xaxis_range))
  expect_equal(sort(vline_trace$y), sort(yaxis_range))
})

test_that("3D scatter labels each scene axis with its own title", {
  p <- plotly_scatterplot(
    x = c(1, 2, 3), y = c(4, 5, 6), z = c(7, 8, 9),
    plot_type = "scatter3d", xlab = "xtitle", ylab = "ytitle", zlab = "ztitle"
  )
  scene <- p$x$layoutAttrs[[1]]$scene

  expect_equal(scene$xaxis$title, "xtitle")
  expect_equal(scene$yaxis$title, "ytitle")
  expect_equal(scene$zaxis$title, "ztitle")
})
