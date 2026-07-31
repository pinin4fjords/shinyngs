test_that("compile_enrichment_overview resolves every available contrast", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)

  expect_equal(nrow(data), 4)
  expect_equal(unique(data$method), "ROAST (rotation gene set test)")
  expect_equal(attr(data, "contrast_levels"), c("Condition: treated vs control", "Batch: b vs a"))
})

test_that("prepare_enrichment_overview retains missing gene-set contrast combinations", {
  eselist <- make_enrichmentoverview_eselist()
  data <- compile_enrichment_overview(eselist[[1]], "counts", "KEGG", eselist@contrasts)
  overview <- prepare_enrichment_overview(data, top_n = 3, max_fdr = 0.1)

  expect_equal(nrow(overview), 6)
  expect_equal(sum(is.na(overview$fdr)), 2)
  expect_equal(attr(overview, "gene_set_levels"), c("SET_A", "SET_C", "SET_B"))
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
      top_n = 20,
      max_fdr = 0.1
    )
    session$elapse(400)

    expect_equal(nrow(getPreparedEnrichmentOverview()), 6)
    expect_false(is.null(output$plot))
    expect_false(is.null(output[["table-datatable"]]))
  })
})
