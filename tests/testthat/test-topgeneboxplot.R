make_topgene_matrix <- function() {
  set.seed(1)
  mat <- matrix(rpois(4 * 6, lambda = 200) + 1, nrow = 4)
  rownames(mat) <- paste0("gene", 1:4)
  colnames(mat) <- paste0("s", 1:6)
  mat
}

# topgeneRankOptions()

test_that("topgeneRankOptions only offers options whose column is available", {
  qp_and_fc <- topgeneRankOptions(c("q value", "p value", "Fold change"))
  expect_setequal(vapply(qp_and_fc, function(o) o$key, character(1)), vapply(topgene_rank_options, function(o) o$key, character(1)))

  fc_only <- topgeneRankOptions("Fold change")
  expect_setequal(vapply(fc_only, function(o) o$key, character(1)), c("fc_abs_desc", "fc_abs_asc", "fc_desc", "fc_asc"))

  expect_length(topgeneRankOptions(character(0)), 0)
})

test_that("the absolute fold change option ranks by magnitude regardless of sign", {
  opt <- Find(function(o) identical(o$key, "fc_abs_desc"), topgene_rank_options)
  fc <- c(-10, 3, -1, 8)

  ranked <- order(opt$transform(fc), decreasing = opt$decreasing)
  expect_equal(fc[ranked], c(-10, 8, 3, -1))
})

# plotly_topgene_boxplots()

test_that("plotly_topgene_boxplots shows an all-zero group as a flat line at zero, not a missing box", {
  # A gene entirely off in one condition is exactly what an absolute
  # fold-change ranking surfaces, and shouldn't make that side's box vanish
  mat <- matrix(c(rep(0, 3), rep(500, 3)), nrow = 1, dimnames = list("gene1", paste0("s", 1:6)))
  groupby <- rep(c("A", "B"), each = 3)

  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat)))
  box_traces <- Filter(function(t) identical(t$type, "box"), built$x$data)
  values_by_group <- stats::setNames(lapply(box_traces, function(t) t$y), vapply(box_traces, function(t) t$name, character(1)))

  expect_length(box_traces, 2)
  expect_true(all(is.finite(values_by_group[["A"]])))
  expect_equal(unique(values_by_group[["A"]]), 0)
})

test_that("plotly_topgene_boxplots draws one box trace per gene per group", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)

  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat)))
  box_traces <- Filter(function(t) identical(t$type, "box"), built$x$data)

  # 4 genes x 2 groups
  expect_length(box_traces, 8)
  expect_setequal(unique(vapply(box_traces, function(t) t$name, character(1))), c("A", "B"))
})

test_that("plotly_topgene_boxplots toggles the beeswarm point overlay", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)

  with_points <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat), beeswarm = TRUE))
  without_points <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat), beeswarm = FALSE))

  expect_true(all(vapply(with_points$x$data, function(t) t$boxpoints, character(1)) == "all"))
  expect_true(all(vapply(without_points$x$data, function(t) t$boxpoints, character(1)) == "outliers"))
})

test_that("plotly_topgene_boxplots renders supplied annotations per facet", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)
  annotations <- stats::setNames(paste0("q value = ", c(0.001, 0.01, 0.02, 0.03)), rownames(mat))

  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat), annotations = annotations))
  xaxis_titles <- unlist(lapply(built$x$layout[grepl("^xaxis", names(built$x$layout))], function(x) x$title))

  for (g in rownames(mat)) {
    expect_true(any(grepl(annotations[[g]], xaxis_titles, fixed = TRUE)))
  }
})

test_that("plotly_topgene_boxplots shows the y axis title on every row, not just the first facet", {
  set.seed(2)
  mat <- matrix(rpois(6 * 4, lambda = 200) + 1, nrow = 6)
  rownames(mat) <- paste0("gene", 1:6)
  colnames(mat) <- paste0("s", 1:4)
  groupby <- rep(c("A", "B"), each = 2)

  # 6 genes at the default ncol (3) means 2 rows of 3; genes 1 and 4 (facets
  # yaxis/yaxis4) start a row, genes 2/3/5/6 don't
  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat)))
  layout <- built$x$layout
  has_title <- function(axis_name) is.character(layout[[axis_name]]$title)

  expect_true(has_title("yaxis"))
  expect_true(has_title("yaxis4"))
  expect_false(has_title("yaxis2"))
  expect_false(has_title("yaxis3"))
  expect_false(has_title("yaxis5"))
  expect_false(has_title("yaxis6"))
})

test_that("plotly_topgene_boxplots keeps the legend inside the figure regardless of row count", {
  groupby <- rep(c("A", "B"), each = 3)

  # legend.y is anchored to the figure container (not the plotting area), so
  # its pixel offset from the bottom should stay constant as more rows are
  # added, rather than scaling with (and eventually exceeding) the fixed
  # bottom margin reserved for it
  for (n_genes in c(1, 4, 9)) {
    mat <- matrix(rpois(n_genes * 6, lambda = 200) + 1, nrow = n_genes, dimnames = list(paste0("gene", 1:n_genes), paste0("s", 1:6)))

    built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat)))
    legend <- built$x$layout$legend
    height <- built$x$layout$height
    margin_b <- built$x$layout$margin$b

    expect_equal(legend$yref, "container")
    legend_px_from_bottom <- legend$y * height
    expect_true(legend_px_from_bottom > 0 && legend_px_from_bottom < margin_b)
  }
})

test_that("plotly_topgene_boxplots titles facets with the supplied label instead of the raw gene id", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)
  labels <- stats::setNames(c("Symbol1", "Symbol2"), c("gene1", "gene2"))

  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat), labels = labels))
  xaxis_titles <- unlist(lapply(built$x$layout[grepl("^xaxis", names(built$x$layout))], function(x) x$title))

  expect_true(any(grepl("Symbol1", xaxis_titles, fixed = TRUE)))
  expect_true(any(grepl("Symbol2", xaxis_titles, fixed = TRUE)))
  # gene3/gene4 have no entry in `labels` and fall back to their raw id
  expect_true(any(grepl("gene3", xaxis_titles, fixed = TRUE)))
  expect_true(any(grepl("gene4", xaxis_titles, fixed = TRUE)))
})

test_that("plotly_topgene_boxplots combines a supplied label and annotation in one facet title", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)
  labels <- stats::setNames("Symbol1", "gene1")
  annotations <- stats::setNames("q value = 0.001", "gene1")

  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat), labels = labels, annotations = annotations))
  xaxis_titles <- unlist(lapply(built$x$layout[grepl("^xaxis", names(built$x$layout))], function(x) x$title))

  expect_true(any(grepl("Symbol1<br>q value = 0.001", xaxis_titles, fixed = TRUE)))
})

test_that("plotly_topgene_boxplots applies the colour-blind palette by default", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)

  built <- plotly::plotly_build(plotly_topgene_boxplots(mat, groupby, rownames(mat)))
  box_traces <- Filter(function(t) identical(t$type, "box"), built$x$data)
  fillcolors <- unique(vapply(box_traces, function(t) t$fillcolor, character(1)))

  expect_setequal(fillcolors, unname(makeColorScale(2)))
})

# ggplot_topgene_boxplots()

test_that("ggplot_topgene_boxplots facets by gene and toggles the beeswarm layer", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)

  with_swarm <- ggplot_topgene_boxplots(mat, groupby, rownames(mat), beeswarm = TRUE)
  expect_s3_class(with_swarm, "ggplot")
  expect_true("PositionQuasirandom" %in% vapply(with_swarm$layers, function(l) class(l$position)[1], character(1)))

  without_swarm <- ggplot_topgene_boxplots(mat, groupby, rownames(mat), beeswarm = FALSE)
  expect_false("PositionQuasirandom" %in% vapply(without_swarm$layers, function(l) class(l$position)[1], character(1)))
})

test_that("ggplot_topgene_boxplots draws a per-facet annotation layer when supplied", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)
  annotations <- stats::setNames(paste0("q value = ", c(0.001, 0.01, 0.02, 0.03)), rownames(mat))

  with_annotations <- ggplot_topgene_boxplots(mat, groupby, rownames(mat), annotations = annotations)
  without_annotations <- ggplot_topgene_boxplots(mat, groupby, rownames(mat))

  geom_classes <- vapply(with_annotations$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% geom_classes)

  geom_classes_none <- vapply(without_annotations$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% geom_classes_none)
})

test_that("ggplot_topgene_boxplots facets by gene id but labels strips with the supplied display name", {
  mat <- make_topgene_matrix()
  groupby <- rep(c("A", "B"), each = 3)
  labels <- stats::setNames(c("Symbol1", "Symbol2"), c("gene1", "gene2"))

  p <- ggplot_topgene_boxplots(mat, groupby, rownames(mat), labels = labels)
  facet_labeller <- p$facet$params$labeller

  strips <- facet_labeller(data.frame(gene = factor(rownames(mat), levels = rownames(mat))))
  expect_equal(as.character(strips$gene), c("Symbol1", "Symbol2", "gene3", "gene4"))
})

# topgeneFacetLabels()

test_that("topgeneFacetLabels falls back to the gene id for genes missing from labels, or when labels is NULL", {
  labels <- c(gene1 = "Symbol1", gene3 = "Symbol3")
  genes <- c("gene1", "gene2", "gene3")

  expect_equal(topgeneFacetLabels(labels, genes), stats::setNames(c("Symbol1", "gene2", "Symbol3"), genes))
  expect_equal(topgeneFacetLabels(NULL, genes), stats::setNames(genes, genes))
})

# topgeneAnnotationData() / topgeneAnnotationVector()

test_that("topgeneAnnotationData keeps only genes present in both annotations and genes", {
  annotations <- c(gene1 = "a", gene3 = "b", geneX = "c")
  ann_df <- topgeneAnnotationData(annotations, c("gene1", "gene2", "gene3"))

  expect_equal(as.character(ann_df$gene), c("gene1", "gene3"))
  expect_equal(ann_df$label, c("a", "b"))
  expect_null(topgeneAnnotationData(NULL, c("gene1")))
})

test_that("topgeneAnnotationVector returns NA for genes without an annotation", {
  annotations <- c(gene1 = "a", gene3 = "b")
  vec <- topgeneAnnotationVector(annotations, c("gene1", "gene2", "gene3"))

  expect_equal(vec, c("a", NA, "b"))
  expect_equal(topgeneAnnotationVector(NULL, c("gene1", "gene2")), c(NA_character_, NA_character_))
})
