make_sampleselect_eselist <- function(with_group_vars = TRUE) {
  mat <- matrix(1:16, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:8)))

  # ExploratorySummarizedExperimentList() auto-detects group_vars from colData
  # (any non-numeric column shared by more than one sample) when group_vars
  # isn't supplied explicitly, so the "no group vars" case needs a colData
  # with no such column rather than just omitting the group_vars argument.
  coldata <- if (with_group_vars) {
    S4Vectors::DataFrame(row.names = colnames(mat), condition = rep(c("ctrl", "treated"), each = 4))
  } else {
    S4Vectors::DataFrame(row.names = colnames(mat))
  }

  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat), colData = coldata,
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )

  if (with_group_vars) {
    ExploratorySummarizedExperimentList(list(counts = ese), group_vars = "condition", default_groupvar = "condition")
  } else {
    ExploratorySummarizedExperimentList(list(counts = ese))
  }
}

test_that("selectSamples returns every column when sampleSelect is 'all', ignoring name/group inputs", {
  eselist <- make_sampleselect_eselist()

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]]),
    {
      session$setInputs(sampleSelect = "all")
      expect_equal(selectSamples(), paste0("s", 1:8))
    }
  )
})

test_that("selectSamples returns exactly the samples picked by name", {
  eselist <- make_sampleselect_eselist()

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]]),
    {
      session$setInputs(sampleSelect = "name", samples = c("s1", "s3"), sampleGroupVal = "ctrl")
      expect_equal(selectSamples(), c("s1", "s3"))
    }
  )
})

test_that("selectSamples returns the samples belonging to the selected group value(s)", {
  eselist <- make_sampleselect_eselist()

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]]),
    {
      session$setInputs(sampleSelect = "group", samples = character(0), sampleGroupVar = "condition", sampleGroupVal = "ctrl")
      expect_equal(selectSamples(), paste0("s", 1:4))
    }
  )
})

test_that("selectSamples treats NA group values as an empty-string group", {
  eselist <- make_sampleselect_eselist()
  SummarizedExperiment::colData(eselist[["counts"]])$condition[1] <- NA

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]]),
    {
      session$setInputs(sampleSelect = "group", samples = character(0), sampleGroupVar = "condition", sampleGroupVal = "")
      expect_equal(selectSamples(), "s1")
    }
  )
})

test_that("getSampleGroupVar reflects the chosen grouping variable", {
  eselist <- make_sampleselect_eselist()

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]]),
    {
      session$setInputs(sampleSelect = "group", sampleGroupVar = "condition", sampleGroupVal = "ctrl")
      expect_equal(getSampleGroupVar(), "condition")
    }
  )
})

test_that("getSummaryType reads the nested summarisematrix input when summarisation is allowed", {
  eselist <- make_sampleselect_eselist()

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]], allow_summarise = TRUE),
    {
      session$setInputs(sampleSelect = "all", "summarise-summaryType" = "colMeans")
      expect_equal(reactives$getSummaryType(), "colMeans")
    }
  )
})

test_that("the returned reactives omit getSummaryType when summarisation is disallowed", {
  eselist <- make_sampleselect_eselist()

  shiny::testServer(
    sampleselect,
    args = list(id = "sampleselect", eselist = eselist, getExperiment = function() eselist[["counts"]], allow_summarise = FALSE),
    {
      session$setInputs(sampleSelect = "all")
      expect_null(reactives$getSummaryType)
    }
  )
})

# sampleselectInput()

test_that("sampleselectInput renders only a hidden 'all' input when select_samples is FALSE", {
  eselist <- make_sampleselect_eselist()

  ui <- sampleselectInput("sampleselect", eselist, getExperiment = function() eselist[["counts"]], select_samples = FALSE)

  rendered <- as.character(ui)
  expect_match(rendered, "display: none", fixed = TRUE)
  expect_no_match(rendered, "<select")
})

test_that("sampleselectInput offers a 'group' selection option when the eselist has group_vars", {
  eselist <- make_sampleselect_eselist(with_group_vars = TRUE)

  ui <- sampleselectInput("sampleselect", eselist, getExperiment = function() eselist[["counts"]])

  rendered <- as.character(ui)
  expect_match(rendered, "sampleGroupVar", fixed = TRUE)
})

test_that("sampleselectInput omits the 'group' selection option when the eselist has no group_vars", {
  eselist <- make_sampleselect_eselist(with_group_vars = FALSE)

  ui <- sampleselectInput("sampleselect", eselist, getExperiment = function() eselist[["counts"]])

  rendered <- as.character(ui)
  expect_no_match(rendered, "sampleGroupVar", fixed = TRUE)
})
