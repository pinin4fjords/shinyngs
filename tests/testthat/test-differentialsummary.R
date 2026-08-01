test_that("interactive_differential_summary plots up and down counts around zero", {
  summary <- data.frame(
    Variable = c("condition", "condition"),
    `group 1` = c("control", "control"),
    `group 2` = c("treated_a", "treated_b"),
    `Differential genes (up)` = c(12, 0),
    `Differential genes (down)` = c(4, 0),
    check.names = FALSE
  )
  built <- plotly::plotly_build(interactive_differential_summary(summary))

  expect_equal(as.numeric(built$x$data[[1]]$x), c(-4, 0))
  expect_equal(as.numeric(built$x$data[[2]]$x), c(12, 0))
  expect_true(all(vapply(built$x$data, function(trace) all(trace$textposition == "none"), logical(1))))
  expect_equal(built$x$layout$barmode, "relative")
  expect_equal(
    vapply(built$x$data, function(trace) trace$marker$color, character(1)),
    unname(DIRECTION_COLORS[c("Down", "Up")])
  )
})

test_that("direction counts ignore neutral and missing fold changes", {
  expect_equal(
    count_differential_directions(c(4, -4, 1, -1, 0, NA_real_)),
    c(up = 1, down = 1)
  )
})

make_differentialsummary_eselist <- function() {
  gene_ids <- paste0("gene", 1:12)
  counts <- matrix(
    stats::rpois(12 * 4, lambda = 50) + 1,
    nrow = 12,
    dimnames = list(gene_ids, paste0("s", 1:4))
  )
  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    grpA = c("ctrl", "ctrl", "treatA", "treatA"),
    grpB = c("ctrl", "ctrl", "treatB", "treatB")
  )
  fold_changes <- matrix(
    c(
      rep(3, 5), rep(-3, 7),
      rep(4, 7), rep(-4, 5)
    ),
    nrow = 12,
    dimnames = list(gene_ids, c("1", "2"))
  )
  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = data.frame(gene_id = gene_ids, row.names = gene_ids),
    idfield = "gene_id",
    contrast_stats = list(counts = list(fold_changes = fold_changes))
  )
  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese), group_vars = c("grpA", "grpB"),
    default_groupvar = "grpA"
  )
  eselist@contrasts <- list(
    list(id = "c1", Variable = "grpA", Group.1 = "ctrl", Group.2 = "treatA"),
    list(id = "c2", Variable = "grpB", Group.1 = "ctrl", Group.2 = "treatB")
  )
  eselist
}

test_that("differentialsummary renders the directional counts and backing table", {
  eselist <- make_differentialsummary_eselist()
  shiny::testServer(differentialsummary, args = list(id = "summary", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    session$setInputs(
      `differentialsummary-experiment` = "counts",
      `differentialsummary-assay` = "counts",
      `differentialsummary-selectmatrix-geneSelect` = "all",
      `differentialsummary-selectmatrix-sampleSelect` = "all",
      `differentialsummary-filterRows` = FALSE,
      `differentialsummary-contrasts-summaryType` = "colMeans",
      `differentialsummary-contrasts0` = c("1", "2")
    )
    session$elapse(400)

    summary <- getDifferentialSummary()
    expect_equal(summary[[grep("\\(up\\)$", colnames(summary), value = TRUE)]], c(5, 7))
    expect_equal(summary[[grep("\\(down\\)$", colnames(summary), value = TRUE)]], c(7, 5))
    expect_false(is.null(output$plot))
    expect_false(is.null(output[["table-datatable"]]))
  })
})
