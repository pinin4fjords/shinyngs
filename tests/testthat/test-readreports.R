make_readreports_eselist <- function() {
  sample_ids <- paste0("s", 1:4)

  read_reports <- list(
    read_attrition = matrix(
      c(
        100, 100, 100, 100,
        90, 88, 92, 85,
        80, 75, 85, 70
      ),
      nrow = 4, dimnames = list(sample_ids, c("raw", "trimmed", "aligned"))
    ),
    other_metric = matrix(
      c(
        50, 40, 60, 45,
        20, 22, 18, 25,
        2, 1, 3, 2
      ),
      nrow = 4, dimnames = list(sample_ids, c("high_count", "mid_count", "low_count"))
    )
  )

  make_medium_module_eselist(read_reports = read_reports)
}

run_readreports_server <- function(eselist, report_type, expr) {
  shiny::testServer(readreports, args = list(id = "readreports", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    session$setInputs(
      "readreports-experiment" = "counts",
      "readreports-assay" = "counts",
      "readreports-selectmatrix-sampleSelect" = "all",
      "readreports-selectmatrix-geneSelect" = "all",
      reportType = report_type,
      "barplot-barMode" = "stack"
    )
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

test_that("getReportTable drops columns with no sample above the reporting threshold and orders the rest by descending mean", {
  eselist <- make_readreports_eselist()

  run_readreports_server(eselist, "other_metric", expr = quote({
    tbl <- getReportTable()

    expect_equal(colnames(tbl), c("High count", "Mid count"))
    expect_equal(unname(tbl[, "High count"]), c(50, 40, 60, 45))
  }))
})

test_that("getReportTable keeps and orders columns that clear the threshold, with prettified names", {
  eselist <- make_readreports_eselist()

  run_readreports_server(eselist, "read_attrition", expr = quote({
    tbl <- getReportTable()

    expect_equal(colnames(tbl), c("Raw", "Trimmed", "Aligned"))
  }))
})

test_that("getPlotmatrix transposes the report table so columns are samples", {
  eselist <- make_readreports_eselist()

  run_readreports_server(eselist, "read_attrition", expr = quote({
    pm <- getPlotmatrix()

    expect_equal(colnames(pm), paste0("s", 1:4))
    expect_equal(rownames(pm), c("Raw", "Trimmed", "Aligned"))
  }))
})

test_that("getDefaultMode is 'overlay' for read_attrition and 'stack' for other report types", {
  eselist <- make_readreports_eselist()

  run_readreports_server(eselist, "read_attrition", expr = quote({
    expect_equal(getDefaultMode(), "overlay")
  }))

  run_readreports_server(eselist, "other_metric", expr = quote({
    expect_equal(getDefaultMode(), "stack")
  }))
})

test_that("readreports renders a bar plot and a table for the selected report type", {
  eselist <- make_readreports_eselist()

  run_readreports_server(eselist, "read_attrition", expr = quote({
    expect_false(is.null(output[["barplot-barPlot"]]))
    expect_false(is.null(output[["readrep-datatable"]]))
  }))
})

test_that("readreports' plot and table titles name the selected, prettified report type", {
  eselist <- make_readreports_eselist()

  run_readreports_server(eselist, "other_metric", expr = quote({
    expect_match(output$plotTitle[[1]], "Other metric plot")
    expect_match(output$tableTitle[[1]], "Other metric data")
  }))
})

# readreportsInput() - filters out experiments without a read_reports slot

test_that("readreportsInput excludes experiments lacking a read_reports slot from the experiment picker", {
  with_reports <- make_readreports_eselist()[["counts"]]
  without_reports <- make_medium_module_eselist(n_genes = 3, n_samples = 2)[["counts"]]

  eselist <- ExploratorySummarizedExperimentList(
    list(has_reports = with_reports, no_reports = without_reports),
    group_vars = "condition", default_groupvar = "condition"
  )

  ui <- readreportsInput("readreports", eselist)

  rendered <- as.character(tagList(ui))
  expect_match(rendered, "has_reports", fixed = TRUE)
  expect_no_match(rendered, "no_reports")
})
