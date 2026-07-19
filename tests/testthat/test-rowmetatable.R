test_that("rowmetatable returns the row metadata of the selected experiment", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(rowmetatable, args = list(id = "rowmetatable", eselist = eselist), {
    session$setInputs(
      "rowmetatable-experiment" = "counts",
      "rowmetatable-assay" = "counts",
      "rowmetatable-selectmatrix-sampleSelect" = "all",
      "rowmetatable-selectmatrix-geneSelect" = "all",
      "categorycount-category" = "biotype",
      "categorycount-fill" = "none"
    )
    session$elapse(400)

    meta <- getRowMeta()
    expect_setequal(colnames(meta), c("gene_id", "gene_name", "biotype"))
    expect_setequal(rownames(meta), paste0("gene", 1:8))
  })
})

test_that("rowmetatable's linked metadata prettifies column names", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(rowmetatable, args = list(id = "rowmetatable", eselist = eselist), {
    session$setInputs(
      "rowmetatable-experiment" = "counts",
      "rowmetatable-assay" = "counts",
      "rowmetatable-selectmatrix-sampleSelect" = "all",
      "rowmetatable-selectmatrix-geneSelect" = "all",
      "categorycount-category" = "biotype",
      "categorycount-fill" = "none"
    )
    session$elapse(400)

    expect_setequal(colnames(getLinkedRowMeta()), c("Gene id", "Gene name", "Biotype"))
  })
})

test_that("rowmetatable's category-count table tallies a row-metadata column", {
  eselist <- make_medium_module_eselist()

  shiny::testServer(rowmetatable, args = list(id = "rowmetatable", eselist = eselist), {
    session$setInputs(
      "rowmetatable-experiment" = "counts",
      "rowmetatable-assay" = "counts",
      "rowmetatable-selectmatrix-sampleSelect" = "all",
      "rowmetatable-selectmatrix-geneSelect" = "all",
      "categorycount-category" = "biotype",
      "categorycount-fill" = "none"
    )
    session$elapse(400)

    expect_false(is.null(output[["categorycount-table-datatable"]]))
  })
})
