test_that("simpletable renders the display matrix as a datatable", {
  df <- data.frame(gene = c("g1", "g2"), value = c(1, 2))

  shiny::testServer(
    simpletable,
    args = list(id = "tbl", displayMatrix = reactive(df), filename = "mytable"),
    {
      session$elapse(400)
      expect_false(is.null(output$datatable))
    }
  )
})

test_that("simpletable's download handler writes downloadMatrix, not displayMatrix, to CSV", {
  display_df <- data.frame(gene = c("g1", "g2"), value = c("<b>1</b>", "<b>2</b>"))
  download_df <- data.frame(gene = c("g1", "g2"), value = c(1, 2))

  shiny::testServer(
    simpletable,
    args = list(
      id = "tbl", displayMatrix = reactive(display_df), downloadMatrix = reactive(download_df),
      filename = "mytable"
    ),
    {
      path <- output$downloadTable
      expect_equal(read.csv(path), download_df)
    }
  )
})

test_that("simpletable falls back to displayMatrix for download when downloadMatrix is not supplied", {
  df <- data.frame(gene = c("g1", "g2"), value = c(1, 2))

  shiny::testServer(
    simpletable,
    args = list(id = "tbl", displayMatrix = reactive(df), filename = "mytable"),
    {
      path <- output$downloadTable
      expect_equal(read.csv(path), df)
    }
  )
})

test_that("simpletable's download filename uses the supplied filename with a .csv extension", {
  df <- data.frame(gene = "g1", value = 1)

  shiny::testServer(
    simpletable,
    args = list(id = "tbl", displayMatrix = reactive(df), filename = "my_custom_name"),
    {
      path <- output$downloadTable
      expect_equal(basename(path), "my_custom_name.csv")
    }
  )
})

test_that("simpletable accepts a reactive filename", {
  df <- data.frame(gene = "g1", value = 1)

  shiny::testServer(
    simpletable,
    args = list(id = "tbl", displayMatrix = reactive(df), filename = reactive("reactive_name")),
    {
      path <- output$downloadTable
      expect_equal(basename(path), "reactive_name.csv")
    }
  )
})

test_that("simpletable's download omits row names by default", {
  df <- data.frame(value = c(1, 2), row.names = c("g1", "g2"))

  shiny::testServer(
    simpletable,
    args = list(id = "tbl", displayMatrix = reactive(df), filename = "mytable"),
    {
      path <- output$downloadTable
      expect_false("X" %in% colnames(read.csv(path)))
    }
  )
})
