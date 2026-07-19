make_groupby_eselist <- function() {
  mat <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  coldata <- S4Vectors::DataFrame(
    row.names = colnames(mat),
    condition = c("ctrl", "ctrl", "treated", "treated"),
    batch = c("b1", "b2", "b1", "b2")
  )
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat), colData = coldata,
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  ExploratorySummarizedExperimentList(list(counts = ese), group_vars = c("condition", "batch"), default_groupvar = "condition")
}

test_that("getGroupby returns the selected grouping variable", {
  eselist <- make_groupby_eselist()

  shiny::testServer(groupby, args = list(id = "grp", eselist = eselist), {
    session$setInputs(groupby = "batch", "groupby-palette_name" = "colorblind")
    expect_equal(getGroupby(), "batch")
  })
})

test_that("getGroupby returns NULL when group_vars is empty (hidden 'NULL' field)", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat), colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese), group_vars = character(0))

  shiny::testServer(groupby, args = list(id = "grp", eselist = eselist), {
    session$setInputs(groupby = "NULL", "groupby-palette_name" = "colorblind")
    expect_null(getGroupby())
  })
})

test_that("getNumberCategories counts the unique values of the selected group in the supplied experiment data", {
  eselist <- make_groupby_eselist()
  coldata <- data.frame(condition = c("ctrl", "ctrl", "treated", "treated"), row.names = paste0("s", 1:4))

  shiny::testServer(
    groupby,
    args = list(id = "grp", eselist = eselist, selectColData = reactive(coldata)),
    {
      session$setInputs(groupby = "condition", "groupby-palette_name" = "colorblind")
      expect_equal(getNumberCategories(), 2)
    }
  )
})

test_that("getPalette returns one colour per category", {
  eselist <- make_groupby_eselist()
  coldata <- data.frame(condition = c("ctrl", "ctrl", "treated", "treated"), row.names = paste0("s", 1:4))

  shiny::testServer(
    groupby,
    args = list(id = "grp", eselist = eselist, selectColData = reactive(coldata)),
    {
      session$setInputs(groupby = "condition", "groupby-palette_name" = "colorblind")
      expect_length(getPalette(), 2)
    }
  )
})

test_that("groupby returns a checkbox-backed multi-selection when multiple = TRUE", {
  eselist <- make_groupby_eselist()

  shiny::testServer(
    groupby,
    args = list(id = "grp", eselist = eselist, multiple = TRUE),
    {
      session$setInputs(groupby = c("condition", "batch"), "groupby-palette_name" = "colorblind")
      expect_equal(getGroupby(), c("condition", "batch"))
    }
  )
})
