# Fixtures: one eselist with no grouping/contrasts/gene sets, one with all
# three populated (and enough contrasts to exercise detailContrasts' cap).

make_minimal_eselist <- function() {
  n_genes <- 6
  n_samples <- 4
  counts <- matrix(seq_len(n_genes * n_samples), nrow = n_genes)
  rownames(counts) <- paste0("gene", seq_len(n_genes))
  colnames(counts) <- paste0("s", seq_len(n_samples))

  # A column with no repeated values gives chooseGroupingVariables() nothing
  # to pick, so the constructor's auto-detected group_vars stays empty.
  coldata <- S4Vectors::DataFrame(row.names = colnames(counts), uid = paste0("u", seq_len(n_samples)))
  annotation <- data.frame(gene_id = rownames(counts), gene_name = paste0("Gene", seq_len(n_genes)), row.names = rownames(counts))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id", labelfield = "gene_name"
  )
  eselist <- ExploratorySummarizedExperimentList(eses = list(counts = ese))

  # The constructor's group_vars[1] fallback turns "no candidate columns" into
  # a stray NA rather than a genuinely empty default_groupvar; clear both
  # slots directly so has_slot_data() reports what this fixture intends.
  eselist@group_vars <- character(0)
  eselist@default_groupvar <- character(0)
  eselist
}

make_full_eselist <- function(n_contrasts = 15) {
  n_genes <- 6
  n_samples <- 4
  counts <- matrix(seq_len(n_genes * n_samples), nrow = n_genes)
  rownames(counts) <- paste0("gene", seq_len(n_genes))
  colnames(counts) <- paste0("s", seq_len(n_samples))

  coldata <- S4Vectors::DataFrame(row.names = colnames(counts), condition = rep(c("ctrl", "treat"), each = n_samples / 2))
  annotation <- data.frame(gene_id = rownames(counts), gene_name = paste0("Gene", seq_len(n_genes)), row.names = rownames(counts))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id", labelfield = "gene_name",
    gene_set_analyses = list(hallmark = list(cond_vs_ctrl = data.frame(`p value` = 0.01, FDR = 0.05, Direction = "Up", check.names = FALSE))),
    gene_set_analyses_tool = list(hallmark = list(cond_vs_ctrl = "roast"))
  )
  eselist <- ExploratorySummarizedExperimentList(eses = list(counts = ese), group_vars = "condition", default_groupvar = "condition")
  eselist@contrasts <- lapply(seq_len(n_contrasts), function(i) list(Variable = "condition", Group.1 = "ctrl", Group.2 = "treat"))
  eselist
}

# summaryTileSpecs()

test_that("summaryTileSpecs omits the groups/contrasts/genesets tiles when that data is absent", {
  specs <- summaryTileSpecs(make_minimal_eselist())
  keys <- vapply(specs, function(s) s$key, character(1))

  expect_setequal(keys, c("samples", "features_counts", "assays"))
})

test_that("summaryTileSpecs includes the groups/contrasts/genesets tiles when that data is present", {
  specs <- summaryTileSpecs(make_full_eselist())
  keys <- vapply(specs, function(s) s$key, character(1))

  expect_setequal(keys, c("samples", "features_counts", "assays", "groups", "contrasts", "genesets"))

  contrasts_spec <- specs[[which(keys == "contrasts")]]
  expect_equal(contrasts_spec$value, 15)
})

# detailSamples()

test_that("detailSamples reports a plain sample count when there's no default groupvar", {
  d <- detailSamples(make_minimal_eselist())

  expect_equal(d$title, "Samples")
  expect_match(as.character(d$body), "4 samples, with no grouping variable defined")
})

test_that("detailSamples breaks samples down by group when a default groupvar exists", {
  d <- detailSamples(make_full_eselist())

  expect_match(d$title, "4 libraries across 2 Condition levels")
  expect_match(as.character(d$body), "shinyngs-bar-row")
})

# detailFeatures() / detailAssays() / detailGroups() / detailGenesets()

test_that("detailFeatures reports the feature count and available assays", {
  eselist <- make_full_eselist()
  d <- detailFeatures(eselist, "counts")

  expect_match(d$title, "6 quantified")
  expect_match(as.character(d$body), "1 assays: Counts")
})

test_that("detailAssays lists each assay as a pill", {
  d <- detailAssays(make_full_eselist())

  expect_match(d$title, "1 matrices")
  expect_match(as.character(d$body), "shinyngs-pill")
})

test_that("detailGroups marks the default groupvar and reports level counts", {
  d <- detailGroups(make_full_eselist())

  expect_match(d$title, "1 variables")
  body_html <- as.character(d$body)
  expect_match(body_html, "default")
  expect_match(body_html, "2 levels")
})

test_that("detailGenesets lists one entry per assay/gene-set-type combination", {
  d <- detailGenesets(make_full_eselist())

  expect_match(as.character(d$body), "gene set analysis available")
})

# detailContrasts()

test_that("detailContrasts caps displayed rows at 12 and notes how many more there are", {
  d <- detailContrasts(make_full_eselist(n_contrasts = 15))
  body_html <- as.character(d$body)

  expect_match(d$title, "15 differential comparisons")
  expect_equal(lengths(regmatches(body_html, gregexpr("<tr>", body_html))), 13) # header + 12 rows
  expect_match(body_html, "\\+ 3 more")
})

test_that("detailContrasts shows every row and no 'more' note when there are 12 or fewer", {
  d <- detailContrasts(make_full_eselist(n_contrasts = 5))
  body_html <- as.character(d$body)

  expect_equal(lengths(regmatches(body_html, gregexpr("<tr>", body_html))), 6) # header + 5 rows
  expect_false(grepl("more", body_html))
})
