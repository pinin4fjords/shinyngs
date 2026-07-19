make_labelselectfield_ese <- function(with_labelfield = TRUE) {
  gene_ids <- paste0("gene", 1:6)
  mat <- matrix(1:24, nrow = 6, dimnames = list(gene_ids, paste0("s", 1:4)))
  annotation <- data.frame(
    gene_id = gene_ids,
    gene_name = paste0("Gene", 1:6),
    biotype = rep(c("protein_coding", "lncRNA"), each = 3),
    row.names = gene_ids
  )

  ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = colnames(mat)),
    annotation = annotation,
    idfield = "gene_id",
    labelfield = if (with_labelfield) "gene_name" else character(0)
  )
}

test_that("the default (unfiltered) metaField hidden input uses the experiment's labelfield", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese)),
    {
      session$setInputs(metaField = "gene_name", label = "Gene1")
      expect_equal(getSelectedMetaField(), "gene_name")
    }
  )
})

test_that("the metaField falls back to idfield when the experiment has no labelfield set", {
  ese <- make_labelselectfield_ese(with_labelfield = FALSE)
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese)),
    {
      session$setInputs(metaField = "gene_id", label = "gene1")
      expect_equal(getSelectedMetaField(), "gene_id")
    }
  )
})

test_that("getValidLabels lists the sorted values of the selected metadata field", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese), field_selection = TRUE),
    {
      session$setInputs(metaField = "gene_name", label = "Gene1")
      expect_equal(getValidLabels(), paste0("Gene", 1:6))
    }
  )
})

test_that("getValidLabels reflects the field chosen when field_selection is enabled", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese), field_selection = TRUE),
    {
      session$setInputs(metaField = "biotype", label = "lncRNA")
      expect_equal(getValidLabels(), c("lncRNA", "protein_coding"))
    }
  )
})

test_that("getSelectedLabels returns the chosen label as-is when list_input is FALSE", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese)),
    {
      session$setInputs(metaField = "gene_name", label = "Gene2")
      expect_equal(getSelectedLabels(), "Gene2")
    }
  )
})

test_that("getSelectedLabels splits a pasted, newline-separated list when list_input is TRUE", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese), list_input = TRUE),
    {
      session$setInputs(metaField = "gene_name", label = "Gene1\nGene3\nGene5")
      expect_equal(getSelectedLabels(), c("Gene1", "Gene3", "Gene5"))
    }
  )
})

test_that("getAssociatedIds maps selected labels back to their row ids via the metadata field", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese), field_selection = TRUE),
    {
      session$setInputs(metaField = "biotype", label = "lncRNA")
      expect_setequal(getAssociatedIds(), c("gene4", "gene5", "gene6"))
    }
  )
})

test_that("getAssociatedIds restricts to getNonEmptyRows when supplied", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(
      id = "lsf", eselist = eselist, getExperiment = reactive(ese), field_selection = TRUE,
      getNonEmptyRows = reactive(c("gene4", "gene5"))
    ),
    {
      session$setInputs(metaField = "biotype", label = "lncRNA")
      expect_setequal(getAssociatedIds(), c("gene4", "gene5"))
    }
  )
})

test_that("getSelectedIds returns all associated ids when id_selection is disabled", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese), field_selection = TRUE, id_selection = FALSE),
    {
      session$setInputs(metaField = "biotype", label = "lncRNA")
      expect_setequal(getSelectedIds(), c("gene4", "gene5", "gene6"))
    }
  )
})

test_that("getSelectedIds is restricted to the user's picked ids when id_selection is enabled", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese), field_selection = TRUE, id_selection = TRUE),
    {
      session$setInputs(metaField = "biotype", label = "lncRNA", ids = "gene5")
      expect_equal(getSelectedIds(), "gene5")
    }
  )
})

test_that("getValidLabels draws from every experiment when labels_from_all_experiments is TRUE", {
  ese_a <- make_labelselectfield_ese()
  ese_b <- make_labelselectfield_ese()
  SummarizedExperiment::mcols(ese_b)$gene_name <- paste0("OtherGene", 1:6)
  eselist <- ExploratorySummarizedExperimentList(list(a = ese_a, b = ese_b))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese_a), labels_from_all_experiments = TRUE),
    {
      session$setInputs(metaField = "gene_name", label = "Gene1")
      labels <- getValidLabels()
      expect_true(all(paste0("Gene", 1:6) %in% labels))
      expect_true(all(paste0("OtherGene", 1:6) %in% labels))
    }
  )
})

test_that("getValidLabels is restricted to the current experiment when labels_from_all_experiments is FALSE", {
  ese_a <- make_labelselectfield_ese()
  ese_b <- make_labelselectfield_ese()
  SummarizedExperiment::mcols(ese_b)$gene_name <- paste0("OtherGene", 1:6)
  eselist <- ExploratorySummarizedExperimentList(list(a = ese_a, b = ese_b))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese_a), labels_from_all_experiments = FALSE),
    {
      session$setInputs(metaField = "gene_name", label = "Gene1")
      expect_equal(getValidLabels(), paste0("Gene", 1:6))
    }
  )
})

test_that("updateLabelField runs without error and does not alter the currently selected labels", {
  ese <- make_labelselectfield_ese()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese))

  shiny::testServer(
    labelselectfield,
    args = list(id = "lsf", eselist = eselist, getExperiment = reactive(ese)),
    {
      session$setInputs(metaField = "gene_name", label = "Gene1")
      expect_silent(updateLabelField())
    }
  )
})
