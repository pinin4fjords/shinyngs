make_labelled_ese <- function() {
  mat <- matrix(1:6, nrow = 3, dimnames = list(c("g1", "g2", "g3"), c("s1", "s2")))
  annotation <- data.frame(
    gene_id = c("g1", "g2", "g3"),
    gene_name = c("GeneA", "GeneB", NA),
    row.names = c("g1", "g2", "g3")
  )
  ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = annotation,
    idfield = "gene_id",
    labelfield = "gene_name"
  )
}

# id_to_label()

test_that("id_to_label combines the label field with the id, separated by sep", {
  ese <- make_labelled_ese()

  expect_equal(id_to_label(c("g1", "g2"), ese), c("GeneA / g1", "GeneB / g2"))
})

test_that("id_to_label falls back to the bare id where the label is NA", {
  ese <- make_labelled_ese()

  expect_equal(id_to_label("g3", ese), "g3")
})

test_that("id_to_label returns ids unchanged when no labelfield is set", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )

  expect_equal(id_to_label(c("g1", "g2"), ese), c("g1", "g2"))
})

test_that("id_to_label accepts a custom separator", {
  ese <- make_labelled_ese()
  expect_equal(id_to_label("g1", ese, sep = "-"), "GeneA-g1")
})

test_that("linkMatrix escapes labels and encodes identifiers", {
  data <- data.frame(gene_id = "A/B?<script>", note = "<script>alert(1)</script>")
  session <- shiny::MockShinySession$new()
  on.exit(session$close())

  linked <- shiny::withReactiveDomain(session, {
    linked <- linkMatrix(data, list(gene_id = "https://example.org/id/"))
    linked
  })

  expect_match(linked$gene_id, "A%2FB%3F%3Cscript%3E", fixed = TRUE)
  expect_match(linked$gene_id, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", linked$gene_id, fixed = TRUE))
  expect_equal(linked$note, data$note)
  expect_equal(attr(linked, "shinyngs_html_columns"), "gene_id")
})

test_that("linkMatrix rejects active URL schemes", {
  data <- data.frame(gene_id = "alert(1)")
  session <- shiny::MockShinySession$new()
  on.exit(session$close())

  shiny::withReactiveDomain(session, {
    expect_error(
      linkMatrix(data, list(gene_id = "javascript:")),
      "must use HTTP, HTTPS, or a relative URL"
    )
  })
})

test_that("linkMatrix escapes URL root attributes", {
  data <- data.frame(gene_id = "g1")
  session <- shiny::MockShinySession$new()
  on.exit(session$close())

  linked <- shiny::withReactiveDomain(session, {
    linkMatrix(data, list(gene_id = '/gene?source=" onmouseover="alert(1)&id='))
  })

  expect_match(linked$gene_id, "source=&quot; onmouseover=&quot;alert(1)&amp;id=g1", fixed = TRUE)
  expect_false(grepl('onmouseover="', linked$gene_id, fixed = TRUE))
})

# convert_ids()

test_that("convert_ids maps row names to a metadata column", {
  ese <- make_labelled_ese()

  expect_equal(convert_ids(c("g1", "g2"), ese, "gene_name"), c("GeneA", "GeneB"))
})

test_that("convert_ids splits and re-joins space-separated multi-value ids", {
  ese <- make_labelled_ese()

  expect_equal(convert_ids("g1 g2", ese, "gene_name"), "GeneA GeneB")
})

test_that("convert_ids can drop unmatched/NA results", {
  ese <- make_labelled_ese()

  expect_equal(convert_ids(c("g1", "g3"), ese, "gene_name", remove_na = TRUE), "GeneA")
})

# single_valid_matrix()

test_that("single_valid_matrix is TRUE for a single experiment with a single assay", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  expect_true(single_valid_matrix(eselist))
})

test_that("single_valid_matrix is FALSE when the single experiment has multiple assays", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat, norm = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  expect_false(single_valid_matrix(eselist))
})

test_that("single_valid_matrix is FALSE for multiple experiments", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  eselist <- ExploratorySummarizedExperimentList(list(a = ese, b = ese))

  expect_false(single_valid_matrix(eselist))
})

test_that("selectmatrix supports callers that disable summarisation", {
  eselist <- make_medium_module_eselist(n_genes = 12)

  shiny::testServer(
    selectmatrix,
    args = list(
      id = "selectmatrix",
      eselist = eselist,
      var_n = 10,
      provide_all_genes = TRUE,
      allow_summarise = FALSE
    ),
    {
      session$setInputs(
        experiment = "counts",
        assay = "counts",
        "selectmatrix-sampleSelect" = "all",
        "selectmatrix-geneSelect" = "all"
      )
      session$elapse(400)

      expect_false(isSummarised())
      expect_equal(dim(selectMatrix()), c(12L, 4L))
    }
  )
})

test_that("selectmatrix uses its first displayed experiment and assay while inputs initialise", {
  eselist <- make_medium_module_eselist(n_genes = 12)

  shiny::testServer(
    selectmatrix,
    args = list(id = "selectmatrix", eselist = eselist),
    {
      expect_equal(getExperimentId(), "counts")
      expect_equal(getAssay(), "counts")
    }
  )
})

test_that("selectmatrix selects all rows when gene controls are disabled", {
  eselist <- make_medium_module_eselist(n_genes = 12)

  shiny::testServer(
    selectmatrix,
    args = list(id = "selectmatrix", eselist = eselist, select_genes = FALSE),
    {
      session$setInputs("selectmatrix-sampleSelect" = "all")
      expect_equal(nrow(selectMatrix()), 12L)
    }
  )
})
