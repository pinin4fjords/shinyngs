test_that("compile_enrichment_overview resolves every available contrast", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)

  expect_equal(nrow(data), 4)
  expect_equal(unique(data$method), "ROAST (rotation gene set test)")
  expect_equal(unique(data$contrast_number), 1:2)
  expect_equal(attr(data, "contrast_levels"), c("Condition: treated vs control", "Batch: b vs a"))
  expect_equal(
    attr(data, "contrast_numbers"),
    c(`Condition: treated vs control` = 1L, `Batch: b vs a` = 2L)
  )
})

test_that("compile_enrichment_overview labels positional legacy contrasts", {
  eselist <- make_enrichmentoverview_eselist()
  eselist@contrasts <- lapply(eselist@contrasts, function(contrast) {
    unname(unlist(contrast[c("Variable", "Group.1", "Group.2")]))
  })

  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)

  expect_equal(unique(data$contrast), c("Condition: treated vs control", "Batch: b vs a"))
  expect_equal(attr(data, "contrast_levels"), c("Condition: treated vs control", "Batch: b vs a"))
})

test_that("prepare_enrichment_overview retains missing gene-set contrast combinations", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)
  overview <- prepare_enrichment_overview(data, top_n = 3, max_fdr = 0.1)

  expect_equal(nrow(overview), 6)
  expect_equal(sum(is.na(overview$fdr)), 2)
  expect_equal(attr(overview, "gene_set_levels"), c("SET_A", "SET_C", "SET_B"))
  expect_equal(
    paste(overview$gene_set_id, overview$contrast),
    c(
      "SET_A Condition: treated vs control", "SET_A Batch: b vs a",
      "SET_C Condition: treated vs control", "SET_C Batch: b vs a",
      "SET_B Condition: treated vs control", "SET_B Batch: b vs a"
    )
  )
})

test_that("enrichment overview table rows match plotted points", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)
  overview <- prepare_enrichment_overview(data, top_n = 3, max_fdr = 0.1)
  table_rows <- enrichment_overview_plot_rows(overview)

  expect_equal(
    paste(table_rows$gene_set_id, table_rows$contrast),
    c(
      "SET_A Condition: treated vs control", "SET_A Batch: b vs a",
      "SET_C Batch: b vs a", "SET_B Condition: treated vs control"
    )
  )
  expect_true(all(is.finite(table_rows$fdr)))
  expect_false(anyNA(table_rows$direction))
})

test_that("prepare_enrichment_overview uses p value to break equal-FDR ties", {
  data <- data.frame(
    gene_set_id = c("SET_A", "SET_Z"),
    contrast = "contrast 1",
    pvalue = c(0.02, 0.001),
    fdr = c(0.01, 0.01),
    direction = "Up",
    method = "ROAST",
    stringsAsFactors = FALSE
  )

  overview <- prepare_enrichment_overview(data)

  expect_equal(attr(overview, "gene_set_levels"), c("SET_Z", "SET_A"))
})

test_that("prepare_enrichment_overview filters and ranks selected contrasts", {
  data <- data.frame(
    gene_set_id = rep(c("SET_A", "SET_B"), each = 3),
    contrast = rep(c("contrast 1", "contrast 2", "contrast 3"), 2),
    pvalue = c(0.001, 0.2, 0.3, 0.04, 0.02, 0.03),
    fdr = c(0.01, 0.3, 0.4, 0.08, 0.04, 0.05),
    direction = "Up",
    method = "ROAST",
    stringsAsFactors = FALSE
  )
  attr(data, "contrast_levels") <- c("contrast 1", "contrast 2", "contrast 3")

  overview <- prepare_enrichment_overview(
    data,
    selected_contrasts = c("contrast 2", "contrast 3"),
    rank_by = "minimum_fdr"
  )

  expect_equal(attr(overview, "contrast_levels"), c("contrast 2", "contrast 3"))
  expect_equal(attr(overview, "gene_set_levels"), "SET_B")
  expect_setequal(unique(overview$contrast), c("contrast 2", "contrast 3"))
})

test_that("prepare_enrichment_overview selects duplicate labels by contrast number", {
  eselist <- make_enrichmentoverview_eselist()
  eselist@contrasts[[2]][c("Variable", "Group.1", "Group.2")] <- eselist@contrasts[[1]][c("Variable", "Group.1", "Group.2")]
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)

  overview <- prepare_enrichment_overview(data, selected_contrasts = 2L)

  expect_equal(unique(overview$contrast_number), 2L)
})

test_that("prepare_enrichment_overview supports each ranking option", {
  data <- data.frame(
    gene_set_id = c("SET_C", "SET_C", "SET_A", "SET_A", "SET_B", "SET_B"),
    contrast = rep(c("contrast 1", "contrast 2"), 3),
    pvalue = c(0.03, 0.04, 0.001, 0.4, 0.02, 0.03),
    fdr = c(0.04, 0.05, 0.01, 0.5, 0.06, 0.07),
    direction = "Up",
    method = "ROAST",
    stringsAsFactors = FALSE
  )

  expect_equal(attr(prepare_enrichment_overview(data, rank_by = "minimum_pvalue"), "gene_set_levels"), c("SET_A", "SET_B", "SET_C"))
  expect_equal(attr(prepare_enrichment_overview(data, rank_by = "significant_contrasts"), "gene_set_levels"), c("SET_C", "SET_B", "SET_A"))
  expect_equal(attr(prepare_enrichment_overview(data, rank_by = "gene_set_name"), "gene_set_levels"), c("SET_A", "SET_B", "SET_C"))
})

test_that("prepare_enrichment_overview rejects mixed enrichment methods", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)
  data$method[1] <- "GSEA"

  expect_error(prepare_enrichment_overview(data), "more than one enrichment method")
})

test_that("interactive_enrichment_overview plots one point per supplied result", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)
  built <- plotly::plotly_build(interactive_enrichment_overview(data, top_n = 3, max_fdr = 0.1))
  trace_colors <- stats::setNames(
    vapply(built$x$data, function(trace) trace$marker$color, character(1)),
    vapply(built$x$data, function(trace) trace$name, character(1))
  )

  expect_equal(sum(vapply(built$x$data, function(trace) length(trace$x), integer(1))), 4)
  expect_equal(built$x$layout$legend$itemsizing, "constant")
  expect_equal(trace_colors, DIRECTION_COLORS[c("Up", "Down")])
})

test_that("has_cross_contrast_enrichment requires two resolved results", {
  eselist <- make_enrichmentoverview_eselist()
  expect_true(has_cross_contrast_enrichment(eselist))

  eselist[[1]]@gene_set_analyses$counts$KEGG[[2]] <- NULL
  expect_false(has_cross_contrast_enrichment(eselist))
})

test_that("has_cross_contrast_enrichment requires finite FDR values", {
  eselist <- make_enrichmentoverview_eselist()
  eselist[[1]]@gene_set_analyses$counts$KEGG[[2]]$FDR <- NA_real_

  expect_false(has_cross_contrast_enrichment(eselist))
})

test_that("has_cross_contrast_enrichment requires compatible enrichment methods", {
  eselist <- make_enrichmentoverview_eselist()
  eselist[[1]]@gene_set_analyses$counts$KEGG$batch_a_b <- data.frame(
    `NOM p-val` = c(0.02, 0.03), `FDR q-val` = c(0.04, 0.06),
    Direction = c("Up", "Down"), row.names = c("SET_A", "SET_C"),
    check.names = FALSE
  )
  eselist[[1]]@gene_set_analyses_tool$counts$KEGG$batch_a_b <- "gsea"

  expect_false(has_cross_contrast_enrichment(eselist))
})

test_that("enrichmentoverview renders its plot and backing table", {
  eselist <- make_enrichmentoverview_eselist()
  shiny::testServer(enrichmentoverview, args = list(id = "overview", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    session$setInputs(
      `expression-experiment` = "genes",
      `expression-assay` = "counts",
      `expression-selectmatrix-sampleSelect` = "all",
      `expression-selectmatrix-geneSelect` = "all",
      gene_set_type = "KEGG",
      selected_contrasts = c("1", "2"),
      rank_by = "minimum_fdr",
      top_n = 20,
      max_fdr = 0.1
    )
    session$elapse(400)

    expect_equal(nrow(getPreparedEnrichmentOverview()), 6)
    expect_equal(nrow(getEnrichmentOverviewTable()), 4)
    expect_false(is.null(output$plot))
    expect_false(is.null(output[["table-datatable"]]))
  })
})

test_that("enrichmentoverview requires at least two selected contrasts", {
  eselist <- make_enrichmentoverview_eselist()
  shiny::testServer(enrichmentoverview, args = list(id = "overview", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    session$setInputs(
      `expression-experiment` = "genes",
      `expression-assay` = "counts",
      `expression-selectmatrix-sampleSelect` = "all",
      `expression-selectmatrix-geneSelect` = "all",
      gene_set_type = "KEGG",
      selected_contrasts = "1",
      rank_by = "minimum_fdr",
      top_n = 20,
      max_fdr = 0.1
    )

    expect_error(getPreparedEnrichmentOverview(), "Select at least two contrasts")
  })
})

test_that("enrichmentoverview uses displayed defaults while dynamic controls initialise", {
  eselist <- make_enrichmentoverview_eselist()
  shiny::testServer(enrichmentoverview, args = list(id = "overview", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    session$setInputs(
      `expression-selectmatrix-sampleSelect` = "all",
      `expression-selectmatrix-geneSelect` = "all",
      rank_by = "minimum_fdr",
      top_n = 20,
      max_fdr = 0.1
    )

    expect_equal(getGeneSetType(), "KEGG")
    expect_equal(nrow(getPreparedEnrichmentOverview()), 6)
  })
})
