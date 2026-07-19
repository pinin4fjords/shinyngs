# Dedicated coverage of differentialtable()'s contrast selection and
# fold-change/p/q-value filtering, using the shared shinytest2_eselist()
# fixture (60 genes x 12 samples, one contrast, with
# fold_changes/pvals/qvals populated).

run_differentialtable_server <- function(eselist, extra_inputs = list(), expr) {
  inputs <- modifyList(
    list(
      "expression-experiment" = "counts",
      "expression-assay" = "counts",
      "expression-selectmatrix-obs" = 60,
      "expression-selectmatrix-sampleSelect" = "all",
      "expression-selectmatrix-geneSelect" = "all",
      "differential-filterRows" = FALSE,
      "differential-contrasts-summaryType" = "colMeans",
      "differential-contrasts0" = "1",
      "differential-combine_operator" = "intersect"
    ),
    extra_inputs
  )

  # differentialtable() hardcodes selectmatrix(..., var_n = 1000); fixtures
  # with fewer genes than that harmlessly warn that the (unused, since
  # geneSelect is "all" here) variance slider's default exceeds its max.
  withCallingHandlers(
    shiny::testServer(differentialtable, args = list(id = "differential", eselist = eselist), {
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

test_that("differentialtable computes an unfiltered table covering every gene when filterRows is off", {
  eselist <- shinytest2_eselist()

  run_differentialtable_server(eselist, expr = quote({
    table <- contrast_reactives$labelledContrastsTable()
    expect_equal(nrow(table), 60)
  }))
})

test_that("differentialtable's fold change cardinality filter matches the raw contrast_stats fold changes", {
  eselist <- shinytest2_eselist()

  run_differentialtable_server(eselist,
    extra_inputs = list(
      "differential-filterRows" = TRUE,
      "differential-fold_change_card0" = ">= or <= -",
      "differential-fold_change0" = 1,
      "differential-p_value_card0" = "<=",
      "differential-p_value0" = 1,
      "differential-q_value_card0" = "<=",
      "differential-q_value0" = 1
    ),
    expr = quote({
      table <- contrast_reactives$labelledContrastsTable()
      fold_changes <- eselist[["counts"]]@contrast_stats$counts$fold_changes[, 1]

      expect_equal(nrow(table), sum(abs(fold_changes) >= 1))
      expect_true(all(abs(table[["Fold change"]]) >= 1))
    })
  )
})

test_that("differentialtable's p value cardinality filter matches the raw contrast_stats p values", {
  eselist <- shinytest2_eselist()

  run_differentialtable_server(eselist,
    extra_inputs = list(
      "differential-filterRows" = TRUE,
      "differential-fold_change_card0" = ">= or <= -",
      "differential-fold_change0" = 0,
      "differential-p_value_card0" = "<=",
      "differential-p_value0" = 0.5,
      "differential-q_value_card0" = "<=",
      "differential-q_value0" = 1
    ),
    expr = quote({
      table <- contrast_reactives$labelledContrastsTable()
      pvals <- eselist[["counts"]]@contrast_stats$counts$pvals[, 1]

      expect_equal(nrow(table), sum(pvals <= 0.5))
    })
  )
})

test_that("differentialtable returns an empty table when filters exclude every row", {
  eselist <- shinytest2_eselist()

  run_differentialtable_server(eselist,
    extra_inputs = list(
      "differential-filterRows" = TRUE,
      "differential-fold_change_card0" = ">= or <= -",
      "differential-fold_change0" = 1000,
      "differential-p_value_card0" = "<=",
      "differential-p_value0" = 1,
      "differential-q_value_card0" = "<=",
      "differential-q_value0" = 1
    ),
    expr = quote({
      table <- contrast_reactives$labelledContrastsTable()
      expect_equal(nrow(table), 0)
    })
  )
})

test_that("differentialtable's rendered UI title tracks the selected assay", {
  eselist <- shinytest2_eselist()

  run_differentialtable_server(eselist, expr = quote({
    rendered <- output$differentialtable
    expect_match(rendered[[1]], "Differential expression in assay: counts")
  }))
})

test_that("differentialtable renders a simpletable datatable of the differential expression results", {
  eselist <- shinytest2_eselist()

  run_differentialtable_server(eselist, expr = quote({
    expect_false(is.null(output[["differentialtable-datatable"]]))
  }))
})

test_that("differentialtable selects among multiple contrasts independently", {
  n_genes <- 12
  counts <- matrix(
    stats::rnbinom(n_genes * 4, mu = 200, size = 5),
    nrow = n_genes, ncol = 4,
    dimnames = list(paste0("gene", seq_len(n_genes)), paste0("s", 1:4))
  )
  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    grpA = c("ctrl", "ctrl", "treatA", "treatA"),
    grpB = c("ctrl", "treatB", "ctrl", "treatB")
  )
  annotation <- data.frame(gene_id = rownames(counts), row.names = rownames(counts))

  fc1 <- matrix(rep(c(3, -3), each = 6), nrow = n_genes, dimnames = list(rownames(counts), "1"))
  fc2 <- matrix(rep(c(-5, 5), each = 6), nrow = n_genes, dimnames = list(rownames(counts), "2"))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id",
    contrast_stats = list(counts = list(fold_changes = cbind(fc1, fc2)))
  )
  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese), group_vars = c("grpA", "grpB"), default_groupvar = "grpA"
  )
  eselist@contrasts <- list(
    list(Variable = "grpA", Group.1 = "ctrl", Group.2 = "treatA"),
    list(Variable = "grpB", Group.1 = "ctrl", Group.2 = "treatB")
  )

  run_differentialtable_server(eselist,
    extra_inputs = list("differential-contrasts0" = "2"),
    expr = quote({
      table <- contrast_reactives$labelledContrastsTable()
      expect_equal(table[["Fold change"]], rep(c(-5, 5), each = 6))
    })
  )
})
