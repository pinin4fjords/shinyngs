test_that("assaydatatable displays the selected assay's matrix, labelled by gene name", {
  eselist <- make_medium_module_eselist(extra_assay = TRUE)

  shiny::testServer(assaydatatable, args = list(id = "assaydatatable", eselist = eselist), {
    session$setInputs(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all"
    )
    session$elapse(400)

    displayed <- selectmatrix_reactives$selectLabelledMatrix()
    expect_setequal(rownames(displayed), paste0("gene", 1:8))
    expect_setequal(colnames(displayed), c("Gene id", "Gene name", "s1", "s2", "s3", "s4"))
  })
})

test_that("assaydatatable switches matrices when a different assay is selected", {
  eselist <- make_medium_module_eselist(extra_assay = TRUE)

  shiny::testServer(assaydatatable, args = list(id = "assaydatatable", eselist = eselist), {
    session$setInputs(
      "expression-experiment" = "counts",
      "expression-assay" = "norm",
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all"
    )
    session$elapse(400)

    expect_equal(selectmatrix_reactives$getAssay(), "norm")
    displayed <- selectmatrix_reactives$selectLabelledMatrix()
    expect_equal(displayed[["s1"]], c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))
  })
})

test_that("assaydatatable restricts the displayed matrix to the selected samples", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(assaydatatable, args = list(id = "assaydatatable", eselist = eselist), {
    session$setInputs(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-sampleSelect" = "name",
      "expression-selectmatrix-samples" = c("s1", "s2"),
      "expression-selectmatrix-summarise-summaryType" = "colMeans",
      "expression-selectmatrix-geneSelect" = "all"
    )
    session$elapse(400)

    displayed <- selectmatrix_reactives$selectLabelledMatrix()
    expect_setequal(colnames(displayed), c("Gene id", "Gene name", "s1", "s2"))
    expect_false(selectmatrix_reactives$isSummarised())
  })
})

test_that("assaydatatable reports matrices summarised from sample groups", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(assaydatatable, args = list(id = "assaydatatable", eselist = eselist), {
    session$setInputs(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-sampleSelect" = "group",
      "expression-selectmatrix-sampleGroupVar" = "condition",
      "expression-selectmatrix-sampleGroupVal" = c("ctrl", "treated"),
      "expression-selectmatrix-summarise-summaryType" = "colMeans",
      "expression-selectmatrix-geneSelect" = "all"
    )
    session$elapse(400)

    expect_true(selectmatrix_reactives$isSummarised())
    expect_equal(colnames(selectmatrix_reactives$selectMatrix()), c("ctrl", "treated"))
  })
})

test_that("assaydatatable's title names the selected assay", {
  eselist <- make_medium_module_eselist(extra_assay = TRUE)

  shiny::testServer(assaydatatable, args = list(id = "assaydatatable", eselist = eselist), {
    session$setInputs(
      "expression-experiment" = "counts",
      "expression-assay" = "norm",
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all"
    )
    session$elapse(400)

    expect_equal(output$assaydatatable_title, "Assay data: norm")
  })
})

test_that("assaydatatable output binds the table in the initial UI", {
  html <- as.character(assaydatatableOutput("assaydatatable"))

  expect_match(html, 'id="assaydatatable-assaydatatable-datatable"', fixed = TRUE)
  expect_match(html, 'id="assaydatatable-assaydatatable_title"', fixed = TRUE)
  expect_false(grepl("shiny-spinner-output-container", html, fixed = TRUE))
})
