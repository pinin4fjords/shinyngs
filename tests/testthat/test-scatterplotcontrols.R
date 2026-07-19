make_scatterplot_matrix <- function() {
  matrix(1:12, nrow = 4, dimnames = list(paste0("s", 1:4), c("PC1", "PC2", "PC3")))
}

test_that("getXAxis/getYAxis/getZAxis return the selected axis columns in 3D mode", {
  m <- make_scatterplot_matrix()

  shiny::testServer(
    scatterplotcontrols,
    args = list(id = "scatter", getDatamatrix = reactive(m)),
    {
      session$setInputs(threedee = "TRUE", xAxis = 1, yAxis = 2, zAxis = 3, showLabels = FALSE, pointSize = 5)
      session$elapse(400)

      expect_equal(getXAxis(), 1)
      expect_equal(getYAxis(), 2)
      expect_equal(getZAxis(), 3)
      expect_true(getThreedee())
    }
  )
})

test_that("getZAxis is NULL in 2D mode even if a zAxis input exists", {
  m <- make_scatterplot_matrix()

  shiny::testServer(
    scatterplotcontrols,
    args = list(id = "scatter", getDatamatrix = reactive(m)),
    {
      session$setInputs(threedee = "FALSE", xAxis = 1, yAxis = 2, showLabels = FALSE, pointSize = 5)
      session$elapse(400)

      expect_false(getThreedee())
      expect_null(getZAxis())
    }
  )
})

test_that("getShowLabels and getPointSize reflect their inputs", {
  m <- make_scatterplot_matrix()

  shiny::testServer(
    scatterplotcontrols,
    args = list(id = "scatter", getDatamatrix = reactive(m)),
    {
      session$setInputs(threedee = "TRUE", xAxis = 1, yAxis = 2, zAxis = 3, showLabels = TRUE, pointSize = 12)
      session$elapse(400)

      expect_true(getShowLabels())
      expect_equal(getPointSize(), 12)
    }
  )
})

test_that("supplying makeColors adds a getScatterPalette reactive to the returned list", {
  m <- make_scatterplot_matrix()

  shiny::testServer(
    scatterplotcontrols,
    args = list(id = "scatter", getDatamatrix = reactive(m), makeColors = reactive(3)),
    {
      session$setInputs(threedee = "FALSE", xAxis = 1, yAxis = 2, showLabels = FALSE, pointSize = 5, "scatterplot-palette_name" = "colorblind")
      session$elapse(400)

      expect_length(reactives$getScatterPalette(), 3)
    }
  )
})

test_that("no getScatterPalette reactive is in the returned list when makeColors is not supplied", {
  m <- make_scatterplot_matrix()

  shiny::testServer(
    scatterplotcontrols,
    args = list(id = "scatter", getDatamatrix = reactive(m)),
    {
      expect_null(reactives$getScatterPalette)
    }
  )
})
