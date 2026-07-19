test_that("experimenttable displays colData with prettified column names for the selected experiment", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(experimenttable, args = list(id = "experimenttable", eselist = eselist), {
    session$setInputs(experiment = "counts", "categorycount-category" = "condition", "categorycount-fill" = "none")
    session$elapse(400)

    experiment <- getExperiment()
    expect_equal(colnames(experiment), "Condition")
    expect_equal(experiment$Condition, c("ctrl", "ctrl", "treated", "treated"))
  })
})

test_that("experimenttable's raw experiment data keeps un-prettified column names", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(experimenttable, args = list(id = "experimenttable", eselist = eselist), {
    session$setInputs(experiment = "counts", "categorycount-category" = "condition", "categorycount-fill" = "none")
    session$elapse(400)

    expect_equal(colnames(getRawExperiment()), "condition")
  })
})

test_that("experimenttable switches to the newly selected experiment when more than one is available", {
  eselist_a <- make_medium_module_eselist()[["counts"]]
  eselist_b <- make_medium_module_eselist(n_genes = 3, n_samples = 2)[["counts"]]
  SummarizedExperiment::colData(eselist_b)$condition <- c("only_b", "only_b")
  eselist <- ExploratorySummarizedExperimentList(list(a = eselist_a, b = eselist_b), group_vars = "condition", default_groupvar = "condition")

  shiny::testServer(experimenttable, args = list(id = "experimenttable", eselist = eselist), {
    session$setInputs(experiment = "b", "categorycount-category" = "condition", "categorycount-fill" = "none")
    session$elapse(400)

    expect_equal(unique(getRawExperiment()$condition), "only_b")
  })
})

test_that("experimenttable's category-count table tallies the selected experiment column", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(experimenttable, args = list(id = "experimenttable", eselist = eselist), {
    session$setInputs(experiment = "counts", "categorycount-category" = "condition", "categorycount-fill" = "none")
    session$elapse(400)

    expect_false(is.null(output[["categorycount-table-datatable"]]))
  })
})

# experimenttableInput()

test_that("experimenttableInput hides the experiment selector behind a fixed value for a single experiment", {
  eselist <- make_medium_module_eselist()

  ui <- experimenttableInput("experiment", eselist)

  rendered <- as.character(ui)
  expect_match(rendered, "experiment-experiment", fixed = TRUE)
  expect_no_match(rendered, "<select")
})

test_that("experimenttableInput offers a real select input when more than one experiment is available", {
  eselist_a <- make_medium_module_eselist()[["counts"]]
  eselist <- ExploratorySummarizedExperimentList(list(a = eselist_a, b = eselist_a))

  ui <- experimenttableInput("experiment", eselist)

  rendered <- as.character(ui)
  expect_match(rendered, "<select", fixed = TRUE)
})
