test_that("modalServer re-evaluates function title and content on each open", {
  titles <- character()
  bodies <- character()
  gene <- "gene A"
  title_fn <- function() {
    titles <<- c(titles, paste("info for", gene))
    tail(titles, 1)
  }
  content_fn <- function() {
    bodies <<- c(bodies, paste("body for", gene))
    tail(bodies, 1)
  }

  shiny::testServer(
    modalServer,
    args = list(id = "geneInfo", title = title_fn, content = content_fn),
    {
      session$setInputs(link = 1)
      gene <<- "gene B"
      session$setInputs(link = 2)
    }
  )

  expect_identical(titles, c("info for gene A", "info for gene B"))
  expect_identical(bodies, c("body for gene A", "body for gene B"))
})

test_that("modalServer accepts a static title and loads default markdown help", {
  expect_silent(
    shiny::testServer(
      modalServer,
      args = list(id = "dendro", title = "Sample clustering dendrogram"),
      {
        session$setInputs(link = 1)
      }
    )
  )
})

test_that("every id in help_modal_ids has a corresponding inlinehelp markdown file", {
  md_paths <- file.path(system.file("inlinehelp", package = "shinyngs"), paste0(help_modal_ids, ".md"))
  missing <- help_modal_ids[!file.exists(md_paths)]

  expect_equal(missing, character(0))
})

test_that("every inlinehelp markdown file corresponds to an id in help_modal_ids", {
  md_files <- list.files(system.file("inlinehelp", package = "shinyngs"), pattern = "\\.md$")
  md_ids <- sub("\\.md$", "", md_files)
  orphaned <- setdiff(md_ids, help_modal_ids)

  expect_equal(orphaned, character(0))
})
