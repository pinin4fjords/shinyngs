make_differentialsummary_eselist <- function() {
  n_genes <- 60
  gene_ids <- paste0("gene", seq_len(n_genes))
  counts <- matrix(stats::rpois(n_genes * 4, lambda = 50) + 1, nrow = n_genes)
  rownames(counts) <- gene_ids
  colnames(counts) <- paste0("s", 1:4)

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    group_a = c("control", "control", "treated", "treated"),
    group_b = c("control", "control", "treated", "treated")
  )
  fold_changes <- matrix(
    c(rep(3, 25), rep(-3, 35), rep(4, 35), rep(-4, 25)),
    nrow = n_genes,
    dimnames = list(gene_ids, c("1", "2"))
  )
  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata,
    annotation = data.frame(gene_id = gene_ids, row.names = gene_ids),
    idfield = "gene_id", contrast_stats = list(counts = list(fold_changes = fold_changes))
  )
  eselist <- ExploratorySummarizedExperimentList(
    eses = list(genes = ese), group_vars = c("group_a", "group_b"), default_groupvar = "group_a"
  )
  eselist@contrasts <- list(
    list(id = "a", Variable = "group_a", Group.1 = "control", Group.2 = "treated"),
    list(id = "b", Variable = "group_b", Group.1 = "control", Group.2 = "treated")
  )
  eselist
}

run_differentialsummary_server <- function(expr) {
  eselist <- make_differentialsummary_eselist()
  shiny::testServer(differentialsummary, args = list(id = "summary", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    session$setInputs(
      `summary-experiment` = "genes",
      `summary-assay` = "counts",
      `summary-selectmatrix-geneSelect` = "all",
      `summary-selectmatrix-sampleSelect` = "all",
      `summary-filterRows` = TRUE,
      `summary-contrasts-summaryType` = "colMeans",
      `summary-contrasts0` = c("1", "2"),
      `summary-fold_change0` = 2,
      `summary-fold_change_card0` = ">= or <= -"
    )
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

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
})

test_that("differentialsummary applies one filter to every selected contrast", {
  run_differentialsummary_server(quote({
    summary <- getDifferentialSummary()
    up_column <- grep("\\(up\\)$", colnames(summary), value = TRUE)
    down_column <- grep("\\(down\\)$", colnames(summary), value = TRUE)

    expect_equal(summary[[up_column]], c(25, 35))
    expect_equal(summary[[down_column]], c(35, 25))
    expect_false(is.null(output$plot))
  }))
})
