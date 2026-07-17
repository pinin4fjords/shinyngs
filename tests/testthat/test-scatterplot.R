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
