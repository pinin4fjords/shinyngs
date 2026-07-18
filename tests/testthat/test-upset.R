# Fixture: 12 genes, two contrasts (grpA: ctrl vs treatA, grpB: ctrl vs
# treatB) with hand-picked fold-change signs so that, once split into
# up/down sets, the four resulting sets and their pairwise overlaps are
# exactly known:
#
#              grpA down (7)   grpA up (5)
# grpB down (5)      3              2
# grpB up   (7)      4              3
#
# i.e. set sizes {7, 5, 5, 7} and non-empty pairwise intersections
# {3, 4, 2, 3}; every other combination (same-contrast up/down, and any
# 3- or 4-way combination) is empty by construction.

make_upset_eselist <- function() {
  n_genes <- 12
  gene_ids <- paste0("gene", seq_len(n_genes))

  set.seed(1)
  counts <- matrix(rpois(n_genes * 4, lambda = 50) + 1, nrow = n_genes)
  rownames(counts) <- gene_ids
  colnames(counts) <- paste0("s", 1:4)

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    grpA = c("ctrl", "ctrl", "treatA", "treatA"),
    grpB = c("ctrl", "ctrl", "treatB", "treatB")
  )
  annotation <- data.frame(gene_id = gene_ids, row.names = gene_ids)

  # (+,+) x3, (+,-) x2, (-,+) x4, (-,-) x3
  fc1 <- c(3, 3, 3, 2, 2, -2, -2, -2, -2, -3, -3, -3)
  fc2 <- c(4, 4, 4, -4, -4, 5, 5, 5, 5, -5, -5, -5)
  fold_changes <- matrix(c(fc1, fc2), nrow = n_genes, dimnames = list(gene_ids, c("1", "2")))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id", contrast_stats = list(counts = list(fold_changes = fold_changes))
  )

  eselist <- ExploratorySummarizedExperimentList(eses = list(counts = ese), group_vars = c("grpA", "grpB"), default_groupvar = "grpA")
  eselist@contrasts <- list(
    list(id = "c1", Variable = "grpA", Group.1 = "ctrl", Group.2 = "treatA"),
    list(id = "c2", Variable = "grpB", Group.1 = "ctrl", Group.2 = "treatB")
  )
  eselist
}

# Drives the upset module up to the point where its reactives are ready to
# read, mirroring the differentialtable testServer test in
# test-shinytest2.R: selectmatrix/contrasts inputs are only rendered
# client-side (via uiOutput()/insertUI()), so testServer needs every one of
# them set explicitly rather than relying on the widget defaults.

run_upset_server <- function(eselist, extra_inputs = list(), expr) {
  args <- c(
    list(
      "upset-experiment" = "counts",
      "upset-assay" = "counts",
      "upset-selectmatrix-geneSelect" = "all",
      "upset-selectmatrix-sampleSelect" = "all",
      "upset-filterRows" = FALSE,
      "upset-contrasts-summaryType" = "colMeans",
      "upset-contrasts0" = c("1", "2"),
      nsets = 4,
      minorder = 1,
      nintersects = 20,
      separate_by_direction = TRUE,
      set_sort = FALSE,
      bar_numbers = FALSE,
      show_empty_intersections = FALSE,
      intersection_assignment_type = "all"
    ),
    extra_inputs
  )
  args <- args[!duplicated(names(args), fromLast = TRUE)]

  shiny::testServer(upset, args = list(id = "upset", eselist = eselist), {
    session$userData$plotFormat <- function() "png"
    do.call(session$setInputs, args)
    session$elapse(400)
    eval(expr, envir = environment())
  })
}

test_that("getValidSets splits each contrast by direction into disjoint, non-empty sets", {
  run_upset_server(make_upset_eselist(), expr = quote({
    vs <- getValidSets()

    expect_setequal(names(vs), c("GrpA.TreatA_vs_Ctrl.down", "GrpA.TreatA_vs_Ctrl.up", "GrpB.TreatB_vs_Ctrl.down", "GrpB.TreatB_vs_Ctrl.up"))
    expect_setequal(vs$GrpA.TreatA_vs_Ctrl.down, paste0("gene", 6:12))
    expect_setequal(vs$GrpA.TreatA_vs_Ctrl.up, paste0("gene", 1:5))
    expect_setequal(vs$GrpB.TreatB_vs_Ctrl.down, c(paste0("gene", 4:5), paste0("gene", 10:12)))
    expect_setequal(vs$GrpB.TreatB_vs_Ctrl.up, c(paste0("gene", 1:3), paste0("gene", 6:9)))
  }))
})

test_that("getMaxSets caps at the number of valid sets found", {
  run_upset_server(make_upset_eselist(), expr = quote({
    expect_equal(getMaxSets(), 4)
  }))
})

test_that("calculateIntersections computes exact overlap sizes with 'all' assignment", {
  run_upset_server(make_upset_eselist(), expr = quote({
    ints <- calculateIntersections()

    # order-1 (the four sets themselves) then order-2 non-empty overlaps;
    # every same-contrast pair and every 3-/4-way combination is empty and
    # so dropped by show_empty_intersections = FALSE
    expect_equal(ints$intersections, c(7, 7, 5, 5, 4, 3, 3, 2))
    expect_true(all(lengths(ints$combinations) <= 2))
  }))
})

test_that("calculateIntersections assigns overlapping members only to their highest-order intersection under 'upset' assignment", {
  run_upset_server(make_upset_eselist(), extra_inputs = list(intersection_assignment_type = "upset"), expr = quote({
    ints <- calculateIntersections()

    # every gene in each of the four sets also falls in exactly one pairwise
    # overlap, so nothing is left over for the sets on their own
    expect_true(all(lengths(ints$combinations) == 2))
    expect_equal(sort(ints$intersections), c(2, 3, 3, 4))
  }))
})

test_that("upsetSetSizeBarChart and upsetIntersectSizeBarChart draw bars matching the computed sizes", {
  run_upset_server(make_upset_eselist(), expr = quote({
    set_chart <- plotly::plotly_build(upsetSetSizeBarChart())$x$data[[1]]
    expect_equal(set_chart$type, "bar")
    expect_equal(as.numeric(set_chart$x), c(7, 5, 5, 7))

    intersect_chart <- plotly::plotly_build(upsetIntersectSizeBarChart())$x$data[[1]]
    expect_equal(intersect_chart$type, "bar")
    expect_equal(as.numeric(intersect_chart$y), c(7, 7, 5, 5, 4, 3, 3, 2))
  }))
})

test_that("upsetIntersectSizeBarChart truncates to the requested number of intersections", {
  run_upset_server(make_upset_eselist(), extra_inputs = list(nintersects = 3), expr = quote({
    intersect_chart <- plotly::plotly_build(upsetIntersectSizeBarChart())$x$data[[1]]
    expect_equal(as.numeric(intersect_chart$y), c(7, 7, 5))
  }))
})

test_that("upsetIntersectSizeBarChart adds a text trace when bar_numbers is enabled", {
  run_upset_server(make_upset_eselist(), extra_inputs = list(bar_numbers = TRUE), expr = quote({
    built <- plotly::plotly_build(upsetIntersectSizeBarChart())

    expect_length(built$x$data, 2)
    expect_equal(built$x$data[[2]]$mode, "text")
    expect_equal(as.numeric(built$x$data[[2]]$text), c(7, 7, 5, 5, 4, 3, 3, 2))
  }))
})

test_that("upsetGrid draws the background grid, set-membership lines and intersection markers as three scatter traces", {
  run_upset_server(make_upset_eselist(), expr = quote({
    built <- plotly::plotly_build(upsetGrid())

    expect_length(built$x$data, 3)
    expect_true(all(vapply(built$x$data, function(t) t$type, character(1)) == "scatter"))
    expect_equal(built$x$layout$yaxis$range, c(0, 4))
  }))
})
