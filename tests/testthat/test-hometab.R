# navLink()

test_that("navLink builds an anchor that shows the target tabPanel client-side", {
  result <- navLink("Sample metadata", "Experiment")
  html <- as.character(result)

  expect_true(grepl("Sample metadata", html, fixed = TRUE))
  expect_true(grepl("data-value=&quot;Experiment&quot;", html, fixed = TRUE))
  expect_true(grepl("tab(&#39;show&#39;)", html, fixed = TRUE))
})

test_that("navLink includes an optional icon and class", {
  result <- navLink("PCA", "pca", class = "my-class", icon = shiny::icon("cube"))
  html <- as.character(result)

  expect_true(grepl('class="my-class"', html, fixed = TRUE))
  expect_true(grepl("fa-cube", html, fixed = TRUE))
})

# homeNavTargets()

test_that("homeNavTargets returns the expected named tab targets", {
  targets <- homeNavTargets()

  expect_equal(
    targets,
    c(
      samples = "Experiment", pca = "pca", assay = "assay_tables",
      differential = "diff_tables", genesets = "geneset_analyses", geneinfo = "geneinfo"
    )
  )
})

# homeTab()

make_hometab_eselist <- function(with_contrasts = FALSE) {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), row.names = c("g1", "g2")),
    idfield = "gene_id"
  )
  contrasts <- if (with_contrasts) list(list(id = "c1")) else list()
  eselist <- ExploratorySummarizedExperimentList(list(counts = ese), title = "My Study", author = "A. Person", description = "A description")
  eselist@contrasts <- contrasts
  eselist
}

test_that("homeTab produces a Home nav_panel with the study title and author", {
  eselist <- make_hometab_eselist()
  result <- homeTab(shiny::NS("app"), eselist)

  html <- as.character(result)
  expect_true(grepl("My Study", html, fixed = TRUE))
  expect_true(grepl("A. Person", html, fixed = TRUE))
  expect_true(grepl("A description", html, fixed = TRUE))
})

test_that("homeTab only links to differential results when contrasts are present", {
  without_contrasts <- as.character(homeTab(shiny::NS("app"), make_hometab_eselist(with_contrasts = FALSE)))
  with_contrasts <- as.character(homeTab(shiny::NS("app"), make_hometab_eselist(with_contrasts = TRUE)))

  expect_false(grepl("Differential results", without_contrasts, fixed = TRUE))
  expect_true(grepl("Differential results", with_contrasts, fixed = TRUE))
})
