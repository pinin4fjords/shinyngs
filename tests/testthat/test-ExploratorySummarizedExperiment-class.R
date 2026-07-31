test_that("ExploratorySummarizedExperiment rounds assays for compact serialization by default", {
  inputs <- make_test_ese_inputs()
  ese <- do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id")))

  expect_equal(SummarizedExperiment::assay(ese, "expression")[1, 1], 1.23)
})

test_that("ExploratorySummarizedExperiment can retain full assay precision", {
  inputs <- make_test_ese_inputs()
  ese <- do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id", assay_digits = NULL)))

  expect_equal(SummarizedExperiment::assay(ese, "expression")[1, 1], 1.2345)
})

test_that("ExploratorySummarizedExperiment validates annotation fields", {
  inputs <- make_test_ese_inputs()

  expect_error(
    do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "missing"))),
    "idfield field 'missing' is absent"
  )
  expect_error(
    do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id", labelfield = c("label", "gene_id")))),
    "labelfield must contain at most one field"
  )
})

test_that("ExploratorySummarizedExperiment reports sample alignment changes", {
  inputs <- make_test_ese_inputs()
  inputs$colData <- rbind(inputs$colData, data.frame(group = "c", row.names = "s3"))

  expect_warning(
    do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id"))),
    "Dropping colData samples absent from the first assay: s3"
  )
})

test_that("ExploratorySummarizedExperiment rejects non-numeric assays", {
  inputs <- make_test_ese_inputs()
  inputs$assays$expression[1, 1] <- "bad"

  expect_error(
    do.call(ExploratorySummarizedExperiment, c(inputs, list(idfield = "gene_id"))),
    "must contain numeric values"
  )
})
