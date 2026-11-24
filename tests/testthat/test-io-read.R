# Write a minimal study to disk and return a config for eselist_from_config().
# `annotated` selects which of the three matrix features get an annotation row.
write_config_study <- function(annotated = c("ENSG1", "ENSG2", "ENSG3")) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- function(name) file.path(dir, name)

  write.csv(
    data.frame(sample = paste0("s", 1:4), condition = rep(c("treated", "control"), each = 2)),
    path("samples.csv"),
    row.names = FALSE
  )
  gene_names <- c(ENSG1 = "GeneA", ENSG2 = "GeneB", ENSG3 = "GeneC")
  write.csv(
    data.frame(gene_id = annotated, gene_name = unname(gene_names[annotated]), biotype = "protein_coding"),
    path("features.csv"),
    row.names = FALSE
  )
  mat <- matrix(1:12, nrow = 3, dimnames = list(names(gene_names), paste0("s", 1:4)))
  write.csv(data.frame(gene_id = rownames(mat), mat, check.names = FALSE), path("matrix.csv"), row.names = FALSE)

  list(
    title = "Example study",
    author = "An Author",
    experiments = list(
      rnaseq = list(
        coldata = list(file = path("samples.csv"), id = "sample"),
        annotation = list(file = path("features.csv"), id = "gene_id", label = "gene_name"),
        expression_matrices = list(expression = list(file = path("matrix.csv"), measure = "Counts"))
      )
    )
  )
}

test_that("eselist_from_config() builds silently when every feature is annotated", {
  config <- write_config_study()

  expect_no_warning(eselist <- eselist_from_config(config, log2_assays = ""))

  annotation <- as.data.frame(SummarizedExperiment::mcols(eselist[[1]]))
  expect_equal(annotation$gene_name, c("GeneA", "GeneB", "GeneC"))
})

test_that("eselist_from_config() keeps matrix features that have no annotation", {
  config <- write_config_study(annotated = c("ENSG1", "ENSG3"))

  expect_warning(
    eselist <- eselist_from_config(config, log2_assays = ""),
    "1 of 3 assay features have no feature annotation.*ENSG2"
  )

  ese <- eselist[[1]]
  expect_equal(rownames(ese), c("ENSG1", "ENSG2", "ENSG3"))
  expect_equal(as.numeric(unlist(SummarizedExperiment::assay(ese)["ENSG2", ])), c(2, 5, 8, 11))

  annotation <- as.data.frame(SummarizedExperiment::mcols(ese))
  expect_equal(annotation$gene_id, c("ENSG1", "ENSG2", "ENSG3"))
  expect_equal(annotation$gene_name, c("GeneA", "ENSG2", "GeneC"))
  expect_equal(annotation$biotype, c("protein_coding", NA, "protein_coding"))
})

test_that("pad_feature_annotation() leaves a complete annotation untouched", {
  annotation <- data.frame(gene_id = c("g1", "g2"), gene_name = c("A", "B"), row.names = c("g1", "g2"))

  expect_no_warning(padded <- pad_feature_annotation(annotation, c("g2", "g1"), "gene_id", "gene_name"))
  expect_identical(padded, annotation)
})

test_that("pad_feature_annotation() works without a label field", {
  annotation <- data.frame(gene_id = "g1", biotype = "lncRNA", row.names = "g1")

  expect_warning(padded <- pad_feature_annotation(annotation, c("g1", "g2"), "gene_id"), "1 of 2")
  expect_equal(padded["g2", "gene_id"], "g2")
  expect_true(is.na(padded["g2", "biotype"]))
})
