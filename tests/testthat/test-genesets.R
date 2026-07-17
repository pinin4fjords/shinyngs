# read_gmt()

test_that("read_gmt parses gene sets, names and gene ids correctly", {
  gmt_content <- c(
    "SET_A\tdescription for A\tgene1\tgene2\tgene3",
    "SET_B\tdescription for B\tgene2\tgene4"
  )
  gmt_file <- tempfile(fileext = ".gmt")
  writeLines(gmt_content, gmt_file)

  gene_sets <- read_gmt(gmt_file)

  expect_type(gene_sets, "list")
  expect_named(gene_sets, c("SET_A", "SET_B"))
  expect_equal(gene_sets$SET_A, c("gene1", "gene2", "gene3"))
  expect_equal(gene_sets$SET_B, c("gene2", "gene4"))

  unlink(gmt_file)
})

test_that("read_gmt returns an empty character vector for a gene set with no genes", {
  gmt_file <- tempfile(fileext = ".gmt")
  writeLines("SET_C\tno genes here", gmt_file)

  gene_sets <- read_gmt(gmt_file)

  expect_named(gene_sets, "SET_C")
  expect_equal(gene_sets$SET_C, character(0))

  unlink(gmt_file)
})

test_that("read_gmt ignores blank lines", {
  gmt_file <- tempfile(fileext = ".gmt")
  writeLines(c(
    "SET_A\tdesc\tgene1\tgene2",
    "",
    "SET_B\tdesc2\tgene3"
  ), gmt_file)

  gene_sets <- read_gmt(gmt_file)

  expect_named(gene_sets, c("SET_A", "SET_B"))

  unlink(gmt_file)
})

test_that("read_gmt errors informatively on duplicate gene set names", {
  gmt_file <- tempfile(fileext = ".gmt")
  writeLines(c(
    "SET_A\tdesc\tgene1",
    "SET_A\tdesc2\tgene2"
  ), gmt_file)

  expect_error(read_gmt(gmt_file), "Duplicate gene set name.*SET_A")

  unlink(gmt_file)
})

# ExploratorySummarizedExperimentList() gene set keying

test_that("ExploratorySummarizedExperimentList keys gene sets by labelfield and filters unmatched ids", {
  annotation <- data.frame(
    gene_id = paste0("gene", 1:4),
    gene_name = paste0("Gene", 1:4),
    row.names = paste0("gene", 1:4)
  )

  counts <- matrix(
    stats::rnbinom(4 * 4, mu = 200, size = 5),
    nrow = 4, ncol = 4,
    dimnames = list(paste0("gene", 1:4), paste0("sample", 1:4))
  )

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    condition = rep(c("control", "treated"), each = 2)
  )

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = coldata,
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name"
  )

  gmt_content <- c(
    "SET_A\tdescription for A\tgene1\tgene2\tgene_not_present",
    "SET_B\tdescription for B\tgene3"
  )
  gmt_file <- tempfile(fileext = ".gmt")
  writeLines(gmt_content, gmt_file)
  gene_sets <- read_gmt(gmt_file)
  unlink(gmt_file)

  eselist <- ExploratorySummarizedExperimentList(
    eses = list(counts = ese),
    group_vars = "condition",
    default_groupvar = "condition",
    gene_set_id_type = "gene_id",
    gene_sets = list(gene_sets)
  )

  keyed <- eselist@gene_sets[["gene_name"]][[1]]

  expect_named(keyed, c("SET_A", "SET_B"))

  # gene_not_present has no match in the annotation and is dropped
  expect_named(keyed$SET_A, c("gene1", "gene2"))
  expect_equal(unname(keyed$SET_A), c("Gene1", "Gene2"))

  expect_named(keyed$SET_B, "gene3")
  expect_equal(unname(keyed$SET_B), "Gene3")
})
