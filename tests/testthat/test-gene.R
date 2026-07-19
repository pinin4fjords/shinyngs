# Plain function coverage: geneBarplot(), geneModelBiotypeColors()
#
# geneModelGenomeInfo() and the igvShiny-missing fallback for the gene model
# view are already covered in test-optional-heavy-deps.R.

test_that("geneBarplot draws one bar-chart panel per row, colored by the grouping variable", {
  expression <- matrix(
    c(10, 20, 30, 40, 5, 15, 25, 35),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("Gene1", "Gene2"), paste0("s", 1:4))
  )
  experiment <- data.frame(
    condition = c("control", "control", "treated", "treated"),
    row.names = paste0("s", 1:4)
  )

  p <- geneBarplot(expression, experiment, colorby = "condition", palette = c("#111111", "#222222"))
  built <- plotly::plotly_build(p)

  bar_traces <- Filter(function(t) identical(t$type, "bar"), built$x$data)
  expect_length(bar_traces, 4)
})

test_that("geneBarplot draws without color grouping when colorby is NULL", {
  expression <- matrix(c(10, 20, 30, 40), nrow = 1, dimnames = list("Gene1", paste0("s", 1:4)))
  experiment <- data.frame(condition = rep(c("control", "treated"), each = 2), row.names = paste0("s", 1:4))

  p <- geneBarplot(expression, experiment, colorby = NULL)
  built <- plotly::plotly_build(p)

  expect_equal(length(built$x$data), 1)
})

test_that("geneModelBiotypeColors returns a fixed biotype-to-color mapping with a default fallback", {
  colors <- geneModelBiotypeColors()

  expect_true(all(c("protein_coding", "default") %in% names(colors)))
  expect_equal(colors$default, "black")
})

# Happy-path testServer coverage of the gene module's reactive graph, using
# the shared shinytest2_eselist() fixture (60 genes x 12 samples, with
# contrasts/contrast_stats populated - see helper-shinytest2.R).

run_gene_server <- function(eselist, extra_inputs = list(), expr) {
  inputs <- modifyList(
    list(
      "gene-experiment" = "counts",
      "gene-assay" = "counts",
      "gene-selectmatrix-sampleSelect" = "name",
      "gene-selectmatrix-samples" = colnames(eselist[[1]]),
      "gene-selectmatrix-sampleGroupVal" = "control",
      "gene-selectmatrix-geneSelect" = "all",
      "gene-metafields" = "gene_name",
      "gene_label-metaField" = "gene_name",
      "gene_label-label" = "Gene1",
      "gene_label-ids" = "gene1",
      "gene-filterRows" = FALSE,
      "gene-contrasts-summaryType" = "colMeans",
      "gene-contrasts0" = "1",
      "gene-combine_operator" = "intersect",
      "gene-groupby" = "condition",
      "gene-groupby-palette_name" = "colorblind"
    ),
    extra_inputs
  )

  # gene() hardcodes selectmatrix(..., var_n = 1000) for its variance-based
  # row selection UI; shinytest2_eselist() only has 60 genes, so the (unused,
  # since geneSelect is "all" here) variance slider harmlessly warns that its
  # default value exceeds its max.
  withCallingHandlers(
    shiny::testServer(gene, args = list(id = "gene", eselist = eselist), {
      session$userData$plotFormat <- function() "png"
      do.call(session$setInputs, inputs)
      session$elapse(400)
      eval(expr, envir = environment())
    }),
    warning = function(w) {
      if (grepl("should be less than or equal to", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

test_that("getSelectedIdsWithData resolves the row id for the selected gene label", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    expect_equal(getSelectedIdsWithData(), "gene1")
  }))
})

test_that("output$title and output$info reflect the selected experiment name", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    title <- output$title
    expect_match(title[[1]], "Counts")

    info <- output$info
    expect_true(any(grepl("Counts", as.character(info))))
  }))
})

test_that("output$barPlot renders a bar chart for the selected gene", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    rendered <- output$barPlot
    expect_false(is.null(rendered))

    parsed <- jsonlite::fromJSON(rendered, simplifyVector = FALSE)
    types <- vapply(parsed$x$data, function(t) if (is.null(t$type)) NA_character_ else t$type, character(1))
    expect_true("bar" %in% types)
  }))
})

test_that("output$model only offers a gene model link when ensembl_species is set", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    expect_null(output$model)
  }))

  eselist_with_species <- shinytest2_eselist()
  eselist_with_species@ensembl_species <- "hsapiens"

  run_gene_server(eselist_with_species, expr = quote({
    rendered <- output$model
    expect_false(is.null(rendered))
  }))
})

test_that("getGeneContrastsTable restricts the contrasts table to the selected gene", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    table <- getGeneContrastsTable()

    expect_true(nrow(table) >= 1)
    expect_true(all(table[[prettifyVariablename("gene_id")]] == "gene1"))
  }))
})

test_that("gene renders a simpletable of the selected gene's contrast data", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    expect_false(is.null(output[["geneContrastsTable-datatable"]]))
  }))
})

test_that("output$geneInfoTable renders the annotation row for the selected gene", {
  run_gene_server(shinytest2_eselist(), expr = quote({
    rendered <- output$geneInfoTable
    expect_false(is.null(rendered))
  }))
})

test_that("getSelectedIdsWithData reports a validation message when the label has no data in the current assay", {
  run_gene_server(shinytest2_eselist(),
    extra_inputs = list("gene_label-label" = "Gene1", "gene_label-ids" = "not_a_real_id"),
    expr = quote({
      err <- tryCatch(getSelectedIdsWithData(), error = function(e) e)
      expect_s3_class(err, "validation")
    })
  )
})
